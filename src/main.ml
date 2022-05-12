open Caml
open Base
open Zanuda_core
open Utils
open METRIC

let groups_of_metrics =
  let open Metrics in
  [ (module Halstead : METRIC.GROUP)
  ; (module LOC : METRIC.GROUP)
  ; (module CyclomaticComplexity : METRIC.GROUP)
  ; (module Function_count : METRIC.GROUP)
  ; (module Cohesion : METRIC.GROUP)
  ; (module Coupling : METRIC.GROUP)
  ]
;;

let metrics_group_id_list =
  List.map groups_of_metrics ~f:(fun (module L : METRIC.GROUP) -> L.metrics_group_id)
;;

let init_show_list =
  metrics_group_id_list |> List.filter ~f:(fun x -> not @@ String.equal x "(test)")
;;

let verbose_metrics = ref init_show_list
let metrics_to_show = ref init_show_list
let cur_section : CollectedMetrics.Item.t option ref = ref None

let change_metrics_lists new_verb_metrics new_metrics_to_show =
  let change list new_list =
    match new_list with
    | None -> ()
    | Some x -> list := x
  in
  change verbose_metrics new_verb_metrics;
  change metrics_to_show new_metrics_to_show
;;

let collect_results ~metrics_group_id ~work_results ~add_result ~add_extra_info =
  let results, extra_info = work_results in
  if List.mem !verbose_metrics metrics_group_id ~equal:String.equal
  then add_extra_info extra_info;
  List.iter results ~f:(fun (str, value) ->
      let cur_metrics = metrics_group_id ^ "_" ^ str in
      if List.exists !metrics_to_show ~f:(fun x ->
             String.is_substring cur_metrics ~substring:x)
      then add_result cur_metrics value)
;;

let collect_function_results func_info (metrics_group_id, work_results) =
  let func_name = func_info.name.name_string in
  collect_results
    ~metrics_group_id
    ~work_results
    ~add_result:
      (CollectedMetrics.add_func_result func_info.filename func_info.in_module func_name)
    ~add_extra_info:
      (CollectedMetrics.add_extra_info_func
         func_info.filename
         func_info.in_module
         func_name)
;;

let collect_module_results mod_info (metrics_group_id, work_results) =
  collect_results
    ~metrics_group_id
    ~work_results
    ~add_result:(CollectedMetrics.add_module_result mod_info.filename mod_info.mod_name)
    ~add_extra_info:
      (CollectedMetrics.add_extra_info_module mod_info.filename mod_info.mod_name)
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let build_cmt_iterator_actions =
  build_iterator
    ~init:(METRIC.default_iterator_actions [])
    ~compose:(fun (group_id, metrics_group_it) acc ->
      { acc with
        end_of_function =
          (fun info ->
            (group_id, metrics_group_it.end_of_function info) :: acc.end_of_function info)
      ; end_of_module =
          (fun info ->
            (group_id, metrics_group_it.end_of_module info) :: acc.end_of_module info)
      ; begin_of_module =
          (fun info ->
            metrics_group_it.begin_of_module info;
            acc.begin_of_module info)
      ; begin_of_function =
          (fun info ->
            metrics_group_it.begin_of_function info;
            acc.begin_of_function info)
      })
    ~f:(fun iter_actions ->
      { (METRIC.default_iterator_actions ()) with
        end_of_function =
          (fun info ->
            List.iter
              (iter_actions.end_of_function info)
              ~f:(collect_function_results info))
      ; end_of_module =
          (fun info ->
            List.iter (iter_actions.end_of_module info) ~f:(collect_module_results info))
      ; begin_of_module =
          (fun info ->
            CollectedMetrics.add_module info.filename info.mod_name;
            iter_actions.begin_of_module info)
      ; begin_of_function =
          (fun info ->
            CollectedMetrics.add_function
              info.filename
              info.in_module
              info.name.name_string;
            iter_actions.begin_of_function info)
      })
;;

let get_action_lists () =
  List.fold
    groups_of_metrics
    ~init:([], [], [], [], [], [])
    ~f:(fun (l1, l2, l3, l4, l5, l6) (module L : METRIC.GROUP) ->
      let cur_cmt, cur_cmti = L.get_iterators () in
      ( (L.metrics_group_id, cur_cmt.actions) :: l1
      , (L.metrics_group_id, cur_cmti.actions) :: l2
      , cur_cmt.run :: l3
      , cur_cmti.run :: l4
      , cur_cmt.collect_delayed_metrics :: l5
      , (L.metrics_group_id, cur_cmt.get_project_extra_info) :: l6 ))
;;

let ( cmt_iter_action_list
    , cmti_iter_action_list
    , cmt_run_list
    , cmti_run_list
    , collect_delayed_metrics_list
    , get_project_extra_info_list )
  =
  let l1, l2, l3, l4, l5, l6 = get_action_lists () in
  ref l1, ref l2, ref l3, ref l4, ref l5, ref l6
;;

let reset_action_lists () =
  let l1, l2, l3, l4, l5, l6 = get_action_lists () in
  cmt_iter_action_list := l1;
  cmti_iter_action_list := l2;
  cmt_run_list := l3;
  cmti_run_list := l4;
  collect_delayed_metrics_list := l5;
  get_project_extra_info_list := l6
;;

let typed_on_structure info modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmt_iterator_actions !cmt_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun run -> run info file_content)
    ~init:(my_iterator iter_info)
    !cmt_run_list
    typedtree
;;

let typed_on_signature info modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmt_iterator_actions !cmti_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun run -> run info file_content)
    ~init:(my_iterator iter_info)
    !cmti_run_list
    typedtree
;;

let with_info filename f =
  Compile_common.with_info
    ~native:false
    ~source_file:filename
    ~tool_name:"asdf" (* TODO: pass right tool name *)
    ~output_prefix:"asdf"
    ~dump_ext:"asdf"
    f
;;

let get_cur_section () =
  match !cur_section with
  | None -> assert false
  | Some x -> x
;;

let process_cmt_typedtree filename modname typedtree =
  if Config.verbose () then printfn "Analyzing file: %s" filename;
  (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;*)
  let cut_filename = cut_build_dir filename in
  CollectedMetrics.add_file (get_cur_section ()) cut_filename;
  let file_content = List.to_array @@ ("" :: Stdio.In_channel.read_lines cut_filename) in
  with_info filename (fun info -> typed_on_structure info modname file_content typedtree)
;;

let process_cmti_typedtree filename modname typedtree =
  if Config.verbose () then printfn "Analyzing file: %s" filename;
  (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.interface typedtree;*)
  let cut_filename = cut_build_dir filename in
  let file_content = List.to_array @@ ("" :: Stdio.In_channel.read_lines cut_filename) in
  with_info filename (fun info -> typed_on_signature info modname file_content typedtree)
;;

let finish_section () =
  match !cur_section with
  | None -> ()
  | Some section ->
    List.iter !collect_delayed_metrics_list ~f:(fun f -> f ());
    List.iter !get_project_extra_info_list ~f:(fun (group_id, f) ->
        if List.mem !verbose_metrics group_id ~equal:String.equal
        then CollectedMetrics.add_extra_info_section section (f ()))
;;

let process_new_executable exe_name =
  finish_section ();
  cur_section := Some (CollectedMetrics.add_executable exe_name);
  reset_action_lists ()
;;

let process_new_library lib_name =
  finish_section ();
  cur_section := Some (CollectedMetrics.add_library lib_name);
  reset_action_lists ()
;;

let () =
  Config.parse_args ();
  change_metrics_lists (Config.verbose_list ()) (Config.metrics_list ());
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dir path ->
      LoadDune.analyze_dir
        ~cmt:process_cmt_typedtree
        ~cmti:process_cmti_typedtree
        ~on_exe:process_new_executable
        ~on_lib:process_new_library
        path;
      finish_section ();
      CollectedMetrics.Printer.report (Config.verbose ()) ()
  in
  ()
;;
