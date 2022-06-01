open Caml
open Base
open Zanuda_core
open Utils
open METRIC
open Build_iterators

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

let process_cmt_typedtree typed_on_structure filename modname typedtree =
  if analyze_section !cur_section_name
  then (
    if Config.verbose () then printfn "Analyzing file: %s" filename;
    (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;*)
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

let process_cmti_typedtree typed_on_signature filename modname typedtree =
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
  let open Build_iterators in
  let params =
    { verbose_metrics = !verbose_metrics
    ; metrics_to_show = !metrics_to_show
    ; cmt_iter_action_list
    ; cmti_iter_action_list
    ; cmt_run_list
    ; cmti_run_list
    }
  in
  let typed_on_structure = get_typed_on_structure params in
  let typed_on_signature = get_typed_on_signature params in
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dir path ->
      LoadDune.analyze_dir
        ~cmt:(process_cmt_typedtree typed_on_structure)
        ~cmti:(process_cmti_typedtree typed_on_signature)
        ~on_exe:process_new_executable
        ~on_lib:process_new_library
        path;
      finish ();
      CollectedMetrics.Printer.report (Config.verbose ()) ()
  in
  ()
;;
