open Caml
open Base
open METRIC
open Utils

type params =
  { verbose_metrics : string list
  ; metrics_to_show : string list
  ; cmt_iter_action_list :
      (string * ((string * metric_result) list * string list) iterator_actions) list
  ; cmti_iter_action_list : unit iterator_actions list
  ; cmt_run_list :
      (Compile_common.info * string option
       -> string array (* file content *)
       -> Tast_iterator.iterator
       -> Tast_iterator.iterator)
      list
  ; cmti_run_list :
      (Compile_common.info * string option
       -> string array (* file content *)
       -> Tast_iterator.iterator
       -> Tast_iterator.iterator)
      list
  }

let collect_results params ~metrics_group_id ~work_results ~add_result ~add_extra_info =
  let results, extra_info = work_results in
  if List.mem params.verbose_metrics metrics_group_id ~equal:String.equal
  then add_extra_info extra_info;
  List.iter results ~f:(fun (str, value) ->
      let cur_metrics = metrics_group_id ^ "_" ^ str in
      if List.exists params.metrics_to_show ~f:(fun x ->
             String.is_substring cur_metrics ~substring:x)
      then add_result cur_metrics value)
;;

let collect_function_results params func_info (metrics_group_id, work_results) =
  let func_name = func_info.name.name_string in
  collect_results
    params
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

let collect_module_results params (mod_info : module_info) (metrics_group_id, work_results)
  =
  collect_results
    params
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

let build_cmt_iterator_actions params =
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
              ~f:(collect_function_results params info))
      ; end_of_module =
          (fun info ->
            List.iter
              (iter_actions.end_of_module info)
              ~f:(collect_module_results params info))
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

let get_typed_on_structure params info exe_name modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmt_iterator_actions params params.cmt_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.structure o)
    ~compose:(fun run -> run (info, exe_name) file_content)
    ~init:(my_iterator iter_info)
    params.cmt_run_list
    typedtree
;;

let get_typed_on_signature params info exe_name modname file_content typedtree =
  let open Compile_common in
  let open My_Tast_iterator in
  let filename = cut_build_dir info.source_file in
  let iter_info =
    make_iterator_context
      ~filename
      ~cur_module:modname
      ~actions:(build_cmti_iterator_actions params.cmti_iter_action_list)
  in
  build_iterator
    ~f:(fun o -> o.Tast_iterator.signature o)
    ~compose:(fun run -> run (info, exe_name) file_content)
    ~init:(my_iterator iter_info)
    params.cmti_run_list
    typedtree
;;
