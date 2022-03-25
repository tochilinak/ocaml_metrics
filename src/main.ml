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

let collect_function_metrics filename func_name =
  let key = filename ^ ":" ^ func_name in
  List.iter function_metrics ~f:(fun (module L : METRIC.GENERAL) ->
      List.iter (L.get_result ()) ~f:(fun (str, value) ->
          CollectedMetrics.add_result (L.metric_id ^ str) key value))
;;

let init_iterator filename =
  let open Typedtree in
  let get_value_name vb =
    match vb.vb_pat.pat_desc with
    | Tpat_var (x, _) -> Ident.name x
    | _ -> Format.sprintf "<Value on %s>" (short_location_str vb.vb_loc)
  in
  let open Tast_iterator in
  { default_iterator with
    structure_item =
      (fun self str_item ->
        match str_item.str_desc with
        | Tstr_value (_, list) ->
          List.iter list ~f:(fun x ->
              if empty_loc x.vb_loc
              then default_iterator.value_binding self x
              else (
                let value_name = get_value_name x in
                reset_function_metrics ();
                CollectedMetrics.add_function filename value_name;
                default_iterator.value_binding self x;
                collect_function_metrics filename value_name))
        | _ -> default_iterator.structure_item self str_item)
  }
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let typed_on_structure info typedtree =
  let open Compile_common in
  let filename = info.source_file in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : METRIC.GENERAL) ->
      L.reset ();
      L.run info)
    ~init:(init_iterator filename)
    (file_metrics @ function_metrics)
    typedtree;
  build_iterator
    ~f:(fun () -> CollectedMetrics.add_file filename)
    ~compose:(fun (module L : METRIC.GENERAL) () ->
      List.iter (L.get_result ()) ~f:(fun (str, value) ->
          CollectedMetrics.add_result (L.metric_id ^ str) filename value))
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
      CollectedMetrics.report Config.verbose ()
  in
  ()
;;
