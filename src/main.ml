open Caml
open Base
open Zanuda_core
open Utils

let groups_of_metrics =
  let open Metrics in
  [ (module Halstead : METRIC.GROUP)
  ; (module LOC : METRIC.GROUP)
  ; (module CyclomaticComplexity : METRIC.GROUP)
  ; (module Function_count : METRIC.GROUP)
  ; (module Cohesion : METRIC.GROUP)
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

let change_metrics_lists new_verb_metrics new_metrics_to_show =
  let change list new_list =
    match new_list with
    | None -> ()
    | Some x -> list := x
  in
  change verbose_metrics new_verb_metrics;
  change metrics_to_show new_metrics_to_show
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let typed_on_structure info modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    { filename
    ; groups_of_metrics
    ; metrics_to_show = !metrics_to_show
    ; verbose_metrics = !verbose_metrics
    ; file_module = modname
    }
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : METRIC.GROUP) ->
      L.reset ();
      L.run info file_content)
    ~init:(my_iterator iter_info)
    groups_of_metrics
    typedtree;
  build_iterator
    ~f:(fun () -> CollectedMetrics.add_file filename)
    ~compose:(fun (module L : METRIC.GROUP) () ->
      My_Tast_iterator.collect_module_results iter_info (module L))
    ~init:()
    groups_of_metrics
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

let process_cmt_typedtree filename dune_modname typedtree =
  if Config.verbose () then printfn "Analyzing file: %s" filename;
  (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;*)
  let modname =
    let str = String.substr_replace_all dune_modname ~pattern:"__" ~with_:"." in
    if String.is_prefix str ~prefix:"Dune.exe"
    then String.substr_replace_first str ~pattern:"Dune.exe" ~with_:"Dune__exe"
    else str
  in
  let file_content =
    List.to_array @@ ("" :: (Stdio.In_channel.read_lines @@ cut_build_dir filename))
  in
  with_info filename (fun info -> typed_on_structure info modname file_content typedtree)
;;

let () =
  Config.parse_args ();
  change_metrics_lists (Config.verbose_list ()) (Config.metrics_list ());
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dir path ->
      LoadDune.analyze_dir ~cmt:process_cmt_typedtree ~cmti:(fun _ _ _ -> ()) path;
      CollectedMetrics.report (Config.verbose ()) ()
  in
  ()
;;
