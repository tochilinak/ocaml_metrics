open Caml
open Base
open Zanuda_core
open Utils
open Tast_iterator
open Typedtree

type iterator_params =
  { filename : string
  ; groups_of_metrics : (module METRIC.GROUP) list
  ; metrics_to_show : string list
  ; verbose_metrics : string list
  }

let before_function info func_info =
  List.iter info.groups_of_metrics ~f:(fun (module L : METRIC.GROUP) ->
      L.before_function func_info)
;;

let collect_results
    info
    ~metrics_group_id
    ~get_result
    ~get_extra_info
    ~add_result
    ~add_extra_info
  =
  List.iter (get_result ()) ~f:(fun (str, value) ->
      let cur_metrics = metrics_group_id ^ "_" ^ str in
      if List.exists info.metrics_to_show ~f:(fun x ->
             String.is_substring cur_metrics ~substring:x)
      then add_result cur_metrics value);
  if List.mem info.verbose_metrics metrics_group_id ~equal:String.equal
  then add_extra_info (get_extra_info ())
;;

let collect_function_results info func_name (module L : METRIC.GROUP) =
  collect_results
    info
    ~metrics_group_id:L.metrics_group_id
    ~get_result:L.get_function_metrics_result
    ~get_extra_info:L.get_function_extra_info
    ~add_result:(CollectedMetrics.add_func_result info.filename info.filename func_name)
    ~add_extra_info:
      (CollectedMetrics.add_extra_info_func info.filename info.filename func_name)
;;

let collect_module_results info (module L : METRIC.GROUP) =
  collect_results
    info
    ~metrics_group_id:L.metrics_group_id
    ~get_result:L.get_module_metrics_result
    ~get_extra_info:L.get_module_extra_info
    ~add_result:(CollectedMetrics.add_module_result info.filename info.filename)
    ~add_extra_info:(CollectedMetrics.add_extra_info_module info.filename info.filename)
;;

let collect_function_metrics info func_name =
  List.iter info.groups_of_metrics ~f:(collect_function_results info func_name)
;;

let get_value_name vb =
  let loc = short_location_str vb.vb_loc in
  match get_vb_name vb with
  | Some x -> Format.sprintf "%s <%s>" (Ident.name x) loc
  | None -> Format.sprintf "<Value on %s>" loc
;;

let function_value_binding info func_info self x =
  let value_name = get_value_name x in
  CollectedMetrics.add_function info.filename info.filename value_name;
  before_function info func_info;
  self.value_binding self x;
  collect_function_metrics info value_name
;;

let my_structure_item info self str_item =
  match str_item.str_desc with
  | Tstr_value (rec_flag, list) ->
    List.iter list ~f:(fun x ->
        if x.vb_loc.loc_ghost
        then self.value_binding self x
        else (
          let func_info = { is_rec = rec_flag_to_bool rec_flag; name = get_vb_name x } in
          function_value_binding info func_info self x))
  | _ -> default_iterator.structure_item self str_item
;;

let my_iterator info =
  CollectedMetrics.add_module info.filename info.filename;
  let open Typedtree in
  { default_iterator with structure_item = my_structure_item info }
;;
