open Caml
open Base
open Zanuda_core
open Utils

let metrics =
  let open Metrics in
  [ (* * *********************** *)
    (module Function_count : METRIC.GENERAL)
  ; (module Structure_item_count : METRIC.GENERAL)
  ; (module Halstead : METRIC.GENERAL) (* * *********************** *)
  ]
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let typed_on_structure info typedtree =
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : METRIC.GENERAL) ->
      L.reset ();
      L.run info)
    ~init:Tast_iterator.default_iterator
    metrics
    typedtree;
  build_iterator
    ~f:(fun () -> ())
    ~compose:(fun (module L : METRIC.GENERAL) () ->
      List.iter (L.get_result ()) ~f:(fun (str, value) ->
          CollectedMetrics.add_result (L.metric_id ^ str) info.source_file value))
    ~init:()
    metrics
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
