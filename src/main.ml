open Caml
open Base
open Zanuda_core
open Utils

let function_metrics =
  let open Metrics in
  [ (module Halstead : METRIC.GENERAL)
  ; (module LOC : METRIC.GENERAL)
  ; (module CyclomaticComplexity : METRIC.GENERAL)
  ]
;;

let file_metrics =
  let open Metrics in
  [ (module Function_count : METRIC.GENERAL)
  ; (module Cohesion : METRIC.GENERAL) (*; (module Experiment : METRIC.GENERAL)*)
  ]
;;

let metrics = function_metrics @ file_metrics
let metric_id_list = List.map metrics ~f:(fun (module L : METRIC.GENERAL) -> L.metric_id)
let verbose_metrics = ref metric_id_list
let metrics_to_show = ref metric_id_list

let change_metric_lists new_verb_metrics new_metrics_to_show =
  let change list new_list =
    match new_list with
    | None -> ()
    | Some x -> list := x
  in
  change verbose_metrics new_verb_metrics;
  change metrics_to_show new_metrics_to_show
;;

let before_function func_info =
  List.iter metrics ~f:(fun (module L : METRIC.GENERAL) -> L.before_function func_info)
;;

let collect_results add_extra_info add_metrics (module L : METRIC.GENERAL) =
  List.iter (L.get_result ()) ~f:(fun (str, value) ->
      let cur_metric = L.metric_id ^ str in
      if List.exists !metrics_to_show ~f:(fun x ->
             String.is_substring cur_metric ~substring:x)
      then add_metrics cur_metric value);
  if List.mem !verbose_metrics L.metric_id ~equal:String.equal
  then add_extra_info (L.extra_info ())
;;

let collect_func_results filename func_name =
  collect_results
    (CollectedMetrics.add_extra_info_func filename func_name)
    (CollectedMetrics.add_func_result filename func_name)
;;

let collect_file_results filename =
  collect_results
    (CollectedMetrics.add_extra_info_file filename)
    (CollectedMetrics.add_file_result filename)
;;

let collect_function_metrics filename func_name =
  List.iter function_metrics ~f:(collect_func_results filename func_name)
;;

let init_iterator filename =
  let open Typedtree in
  let get_value_name vb =
    let loc = short_location_str vb.vb_loc in
    match get_vb_name vb with
    | Some x -> Format.sprintf "%s <%s>" (Ident.name x) loc
    | None -> Format.sprintf "<Value on %s>" loc
  in
  let open Tast_iterator in
  let function_value_binding rec_flag self x =
    let value_name = get_value_name x in
    let func_info = { is_rec = rec_flag_to_bool rec_flag; name = get_vb_name x } in
    CollectedMetrics.add_function filename value_name;
    before_function func_info;
    self.value_binding self x;
    collect_function_metrics filename value_name
  in
  { default_iterator with
    structure_item =
      (fun self str_item ->
        match str_item.str_desc with
        | Tstr_value (rec_flag, list) ->
          List.iter list ~f:(fun x ->
              if empty_loc x.vb_loc
              then self.value_binding self x
              else function_value_binding rec_flag self x)
        | _ -> default_iterator.structure_item self str_item)
  }
;;

let build_iterator ~init ~compose ~f xs =
  let o = List.fold_left ~f:(fun acc lint -> compose lint acc) ~init xs in
  f o
;;

let typed_on_structure info file_content typedtree =
  let open Compile_common in
  let filename = cut_build_dir info.source_file in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun (module L : METRIC.GENERAL) ->
      L.reset ();
      L.run info file_content)
    ~init:(init_iterator filename)
    metrics
    typedtree;
  build_iterator
    ~f:(fun () -> CollectedMetrics.add_file filename)
    ~compose:(fun (module L : METRIC.GENERAL) () ->
      collect_file_results filename (module L))
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

let read_file filename =
  let lines : string Queue.t = Queue.create () in
  let ic = open_in filename in
  (try
     Queue.enqueue lines "";
     while true do
       Queue.enqueue lines (input_line ic)
     done
   with
  | End_of_file -> close_in ic);
  Queue.to_array lines
;;

let process_cmt_typedtree filename typedtree =
  if Config.verbose () then printfn "Analyzing file: %s" filename;
  (*Format.printf "Typedtree ML:\n%a\n%!" Printtyped.implementation typedtree;*)
  let file_content = read_file @@ cut_build_dir filename in
  with_info filename (fun info -> typed_on_structure info file_content typedtree)
;;

let () =
  Config.parse_args ();
  change_metric_lists (Config.verbose_list ()) (Config.metrics_list ());
  let () =
    match Config.mode () with
    | Config.Unspecified -> ()
    | Dir path ->
      LoadDune.analyze_dir ~cmt:process_cmt_typedtree ~cmti:(fun _ _ -> ()) path;
      CollectedMetrics.report (Config.verbose ()) ()
  in
  ()
;;
