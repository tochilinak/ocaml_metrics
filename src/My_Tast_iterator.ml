open Caml
open Base
open Zanuda_core
open Utils
open METRIC
open Tast_iterator
open Typedtree

type iterator_context =
  { filename : string
  ; mutable cur_module : string
  ; actions : unit METRIC.iterator_actions
  ; mutable inside_module_binding : bool (* default: false *)
  ; mutable module_binding_name : string (* default: "" *)
  ; mutable in_root_structure : bool (* default: true *)
  }

let make_iterator_context ~filename ~cur_module ~actions =
  { filename
  ; cur_module
  ; actions
  ; inside_module_binding = false
  ; module_binding_name = ""
  ; in_root_structure = true
  }
;;

let get_value_name vb =
  let loc = short_location_str vb.vb_loc in
  match get_vb_name_string vb with
  | Some x -> Format.sprintf "%s <%s>" x loc
  | None -> Format.sprintf "<Value on %s>" loc
;;

let function_value_binding ctx func_info self x =
  ctx.actions.begin_of_function func_info;
  self.value_binding self x;
  ctx.actions.end_of_function func_info
;;

let my_value_bindings ctx rec_flag self list =
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
          ; filename = ctx.filename
          ; in_module = ctx.cur_module
          }
        in
        function_value_binding ctx func_info self x))
;;

let my_structure_item ctx self str_item =
  match str_item.str_desc with
  | Tstr_value (rec_flag, list) -> my_value_bindings ctx rec_flag self list
  | _ -> default_iterator.structure_item self str_item
;;

let my_module_expr ctx self mod_expr =
  let get_module_name { mod_desc; mod_loc; _ } =
    if mod_loc.loc_ghost
    then None
    else (
      match mod_desc with
      | Tmod_structure _ ->
        if ctx.inside_module_binding
        then (
          ctx.inside_module_binding <- false;
          Some (ctx.module_binding_name, false))
        else
          Some
            ( ctx.cur_module
              ^ Format.sprintf ".<module at %s>"
              @@ short_location_str mod_loc
            , true )
      | Tmod_functor _ | Tmod_constraint _ -> None
      | _ ->
        ctx.inside_module_binding <- false;
        None)
  in
  match get_module_name mod_expr with
  | Some (x, is_anonymous) ->
    let old_modname = ctx.cur_module in
    ctx.cur_module <- x;
    let mod_info = { mod_name = ctx.cur_module; filename = ctx.filename; is_anonymous } in
    ctx.actions.begin_of_module mod_info;
    default_iterator.module_expr self mod_expr;
    ctx.actions.end_of_module mod_info;
    ctx.cur_module <- old_modname
  | None -> default_iterator.module_expr self mod_expr
;;

let my_module_binding ctx self mb =
  match mb.mb_id with
  | None -> default_iterator.module_binding self mb
  | Some x ->
    ctx.module_binding_name <- ctx.cur_module ^ "." ^ Ident.name x;
    ctx.inside_module_binding <- true;
    default_iterator.module_binding self mb
;;

let my_structure ctx self str =
  if ctx.in_root_structure
  then (
    ctx.in_root_structure <- false;
    let mod_info =
      { mod_name = ctx.cur_module; filename = ctx.filename; is_anonymous = false }
    in
    ctx.actions.begin_of_module mod_info;
    default_iterator.structure self str;
    ctx.actions.end_of_module mod_info)
  else default_iterator.structure self str
;;

(*let my_module_declaration ctx self mod_decl =
  let old_module = ctx.cur_module in
  ctx.cur_module <- ctx.cur_module ^ "." ^ ()*)

let my_iterator ctx =
  let open Typedtree in
  { default_iterator with
    structure = my_structure ctx
  ; structure_item = my_structure_item ctx
  ; module_binding = my_module_binding ctx
  ; module_expr = my_module_expr ctx
  }
;;
