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
let sections_to_analyse = ref None
let cur_section : CollectedMetrics.Item.t option ref = ref None
let cur_section_name = ref ""

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

let collect_module_results (mod_info : module_info) (metrics_group_id, work_results) =
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

let build_cmti_iterator_actions =
  build_iterator
    ~init:(METRIC.default_iterator_actions ())
    ~compose:(fun metrics_group_it acc ->
      { acc with
        end_of_function_sig =
          (fun info ->
            metrics_group_it.end_of_function_sig info;
            acc.end_of_function_sig info)
      ; end_of_module_sig =
          (fun info ->
            metrics_group_it.end_of_module_sig info;
            acc.end_of_module_sig info)
      ; begin_of_module_sig =
          (fun info ->
            metrics_group_it.begin_of_module_sig info;
            acc.begin_of_module_sig info)
      ; begin_of_function_sig =
          (fun info ->
            metrics_group_it.begin_of_function_sig info;
            acc.begin_of_function_sig info)
      })
    ~f:(fun x -> x)
;;

let ( cmt_iter_action_list
    , cmti_iter_action_list
    , cmt_run_list
    , cmti_run_list
    , collect_delayed_metrics_list
    , get_project_extra_info_list )
  =
  List.fold
    groups_of_metrics
    ~init:([], [], [], [], [], [])
    ~f:(fun (l1, l2, l3, l4, l5, l6) (module L : METRIC.GROUP) ->
      let builder = L.get_iterator_builder () in
      ( (L.metrics_group_id, builder.cmt.actions) :: l1
      , builder.cmti.actions :: l2
      , builder.cmt.run :: l3
      , builder.cmti.run :: l4
      , builder.collect_delayed_metrics :: l5
      , (L.metrics_group_id, builder.get_project_extra_info) :: l6 ))
;;

let typed_on_structure info exe_name modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmt_iterator_actions cmt_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun run -> run (info, exe_name) file_content)
    ~init:(my_iterator iter_info)
    cmt_run_list
    typedtree
;;

let typed_on_signature info exe_name modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmti_iterator_actions cmti_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun run -> run (info, exe_name) file_content)
    ~init:(my_iterator iter_info)
    cmti_run_list
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

let get_exe_name () =
  match !cur_section with
  | Some (Executable (x, id)) -> Some (Format.sprintf "%s/%d" x id)
  | _ -> None
;;

let glob_modname modname =
  match !cur_section with
  | Some (Executable (x, id)) -> Format.sprintf "%s/%d" x id ^ "." ^ modname
  | Some (Library _) -> modname
  | _ -> assert false
;;

let analyze_section section =
  match !sections_to_analyse with
  | None -> true
  | Some list -> List.mem list section ~equal:String.equal
;;

let process_cmt_typedtree filename modname typedtree =
  if analyze_section !cur_section_name
  then (
    if Config.verbose () then printfn "Analyzing file: %s" filename;
    Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;
    let cut_filename = cut_build_dir filename in
    CollectedMetrics.add_file (get_cur_section ()) cut_filename;
    let file_content =
      List.to_array @@ ("" :: Stdio.In_channel.read_lines cut_filename)
    in
    with_info filename (fun info ->
        typed_on_structure
          info
          (get_exe_name ())
          (glob_modname modname)
          file_content
          typedtree))
;;

let process_cmti_typedtree filename modname typedtree =
  if analyze_section !cur_section_name
  then (
    if Config.verbose () then printfn "Analyzing file: %s" filename;
    (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.interface typedtree;*)
    let cut_filename = cut_build_dir filename in
    let file_content =
      List.to_array @@ ("" :: Stdio.In_channel.read_lines cut_filename)
    in
    with_info filename (fun info ->
        typed_on_signature
          info
          (get_exe_name ())
          (glob_modname modname)
          file_content
          typedtree))
;;

let finish () =
  List.iter collect_delayed_metrics_list ~f:(fun f -> f ());
  List.iter get_project_extra_info_list ~f:(fun (group_id, f) ->
      if List.mem !verbose_metrics group_id ~equal:String.equal
      then CollectedMetrics.add_extra_info_project (f ()))
;;

let process_new_executable exe_name =
  if analyze_section exe_name
  then cur_section := Some (CollectedMetrics.add_executable exe_name);
  cur_section_name := exe_name
;;

let process_new_library lib_name =
  if analyze_section lib_name
  then cur_section := Some (CollectedMetrics.add_library lib_name);
  cur_section_name := lib_name
;;

let () =
  Config.parse_args ();
  change_metrics_lists (Config.verbose_list ()) (Config.metrics_list ());
  sections_to_analyse := Config.sections_to_analyse ();
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
      finish ();
      CollectedMetrics.Printer.report (Config.verbose ()) ()
  in
  ()
;;
