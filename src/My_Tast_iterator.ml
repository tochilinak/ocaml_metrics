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
  ; mutable cur_module : string
  ; mutable inside_module_binding : bool (* default: false *)
  ; mutable in_root_structure : bool (* default: true *)
  }

let before_function info func_info =
  List.iter info.groups_of_metrics ~f:(fun (module L : METRIC.GROUP) ->
      L.before_function func_info)
;;

let before_module info mod_info =
  List.iter info.groups_of_metrics ~f:(fun (module L : METRIC.GROUP) ->
      L.before_module mod_info)
;;

let collect_results
    info
    ~metrics_group_id
    ~get_result
    ~get_extra_info
    ~add_result
    ~add_extra_info
  =
  if List.mem info.verbose_metrics metrics_group_id ~equal:String.equal
  then add_extra_info (get_extra_info ());
  List.iter (get_result ()) ~f:(fun (str, value) ->
      let cur_metrics = metrics_group_id ^ "_" ^ str in
      if List.exists info.metrics_to_show ~f:(fun x ->
             String.is_substring cur_metrics ~substring:x)
      then add_result cur_metrics value)
;;

let collect_function_results info func_name (module L : METRIC.GROUP) =
  collect_results
    info
    ~metrics_group_id:L.metrics_group_id
    ~get_result:L.get_function_metrics_result
    ~get_extra_info:L.get_function_extra_info
    ~add_result:(CollectedMetrics.add_func_result info.filename info.cur_module func_name)
    ~add_extra_info:
      (CollectedMetrics.add_extra_info_func info.filename info.cur_module func_name)
;;

let collect_module_results info (module L : METRIC.GROUP) =
  collect_results
    info
    ~metrics_group_id:L.metrics_group_id
    ~get_result:L.get_module_metrics_result
    ~get_extra_info:L.get_module_extra_info
    ~add_result:(CollectedMetrics.add_module_result info.filename info.cur_module)
    ~add_extra_info:(CollectedMetrics.add_extra_info_module info.filename info.cur_module)
;;

let collect_function_metrics info func_name =
  List.iter info.groups_of_metrics ~f:(collect_function_results info func_name)
;;

let collect_module_metrics info =
  List.iter info.groups_of_metrics ~f:(collect_module_results info)
;;

let get_value_name vb =
  let loc = short_location_str vb.vb_loc in
  match get_vb_name_string vb with
  | Some x -> Format.sprintf "%s <%s>" x loc
  | None -> Format.sprintf "<Value on %s>" loc
;;

let function_value_binding info func_info self x =
  let value_name = func_info.name.name_string in
  CollectedMetrics.add_function info.filename info.cur_module value_name;
  before_function info func_info;
  self.value_binding self x;
  collect_function_metrics info value_name
;;

let my_value_bindings info rec_flag self list =
  let get_name vb =
    { name_ident_list = get_vb_name_list vb; name_string = get_value_name vb }
  in
  let block = List.map list ~f:get_name in
  List.iteri list ~f:(fun i x ->
      if x.vb_loc.loc_ghost
      then self.value_binding self x
      else (
        let func_info =
          { is_rec = rec_flag_to_bool rec_flag
          ; name = get_name x
          ; block
          ; ind_inside_block = i
          }
        in
        function_value_binding info func_info self x))
;;

let my_structure_item info self str_item =
  match str_item.str_desc with
  | Tstr_value (rec_flag, list) -> my_value_bindings info rec_flag self list
  | _ -> default_iterator.structure_item self str_item
;;

let my_module_expr info self mod_expr =
  let is_named_module_ mod_desc =
    match mod_desc with
    | Tmod_structure _ -> info.inside_module_binding
    | _ -> false
  in
  let is_named_module = is_named_module_ mod_expr.mod_desc in
  if is_named_module
  then (
    CollectedMetrics.add_module info.filename info.cur_module;
    before_module info { mod_name = info.cur_module });
  default_iterator.module_expr self mod_expr;
  if is_named_module then collect_module_metrics info
;;

let my_module_binding info self mb =
  match mb.mb_id with
  | None -> default_iterator.module_binding self mb
  | Some x ->
    let old_cur_module = info.cur_module in
    info.cur_module <- info.cur_module ^ "." ^ Ident.name x;
    info.inside_module_binding <- true;
    default_iterator.module_binding self mb;
    info.inside_module_binding <- false;
    info.cur_module <- old_cur_module
;;

let my_structure info self str =
  let is_root = info.in_root_structure in
  info.in_root_structure <- false;
  if is_root
  then (
    CollectedMetrics.add_module info.filename info.cur_module;
    before_module info { mod_name = info.cur_module });
  default_iterator.structure self str;
  if is_root then collect_module_metrics info
;;

let my_iterator info =
  CollectedMetrics.add_file info.filename;
  let open Typedtree in
  { default_iterator with
    structure = my_structure info
  ; structure_item = my_structure_item info
  ; module_binding = my_module_binding info
  ; module_expr = my_module_expr info
  }
;;
