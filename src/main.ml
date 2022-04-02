open Caml
open Base
open Zanuda_core
open Utils

let function_metrics =
  let open Metrics in
  [ (module Halstead : METRIC.GENERAL) ]
;;

let file_metrics =
  let open Metrics in
  [ (module Function_count : METRIC.GENERAL) (*; (module Experiment : METRIC.GENERAL)*) ]
;;

let reset_function_metrics () =
  List.iter function_metrics ~f:(fun (module L : METRIC.GENERAL) -> L.reset ())
;;

let collect_results where (module L : METRIC.GENERAL) =
  List.iter (L.get_result ()) ~f:(fun (str, value) ->
      CollectedMetrics.add_result (L.metric_id ^ str) where value);
  CollectedMetrics.add_extra_info where (L.extra_info ())
;;

let collect_function_metrics filename func_name =
  let key = filename ^ ":" ^ func_name in
  List.iter function_metrics ~f:(collect_results key)
;;

let outer_iterator filename fallback =
  let open Typedtree in
  let get_value_name vb =
    let loc = short_location_str vb.vb_loc in
    match vb.vb_pat.pat_desc with
    | Tpat_var (x, _) -> Format.sprintf "%s <%s>" (Ident.name x) loc
    | _ -> Format.sprintf "<Value on %s>" loc
  in
  let open Tast_iterator in
  let function_value_binding self x =
    let value_name = get_value_name x in
    CollectedMetrics.add_function filename value_name;
    reset_function_metrics ();
    fallback.value_binding self x;
    collect_function_metrics filename value_name
  in
  { fallback with
    structure_item =
      (fun self str_item ->
        match str_item.str_desc with
        | Tstr_value (_, list) ->
          List.iter list ~f:(fun x ->
            if empty_loc x.vb_loc
            then fallback.value_binding self x
            else function_value_binding self x)
        | _ -> fallback.structure_item self str_item)
  }
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let typed_on_structure info typedtree =
  let open Compile_common in
  let filename =
    String.chop_prefix_if_exists info.source_file ~prefix:"_build/default/"
  in
  build_iterator
    ~f:(fun o ->
      let result_iterator = outer_iterator filename o in
      result_iterator.Tast_iterator.structure result_iterator)
    ~compose:(fun (module L : METRIC.GENERAL) ->
      L.reset ();
      L.run info)
    ~init:(Tast_iterator.default_iterator)
    (file_metrics @ function_metrics)
    typedtree;
  build_iterator
    ~f:(fun () -> CollectedMetrics.add_file filename)
    ~compose:(fun (module L : METRIC.GENERAL) () ->
      collect_results filename (module L : METRIC.GENERAL))
    ~init:()
    file_metrics
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

let process_cmt_typedtree filename typedtree =
  if Config.verbose () then printfn "Analyzing file: %s" filename;
  (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;*)
  with_info filename (fun info -> typed_on_structure info typedtree)
;;

let () =
  Config.parse_args ();
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dir path ->
      LoadDune.analyze_dir ~cmt:process_cmt_typedtree ~cmti:(fun _ _ -> ()) path;
      CollectedMetrics.report (Config.verbose ()) ()
  in
  ()
;;
