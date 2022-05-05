open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
module Ident_Hashtbl = Caml.Hashtbl.Make (Ident.T)

let metrics_group_id = "coupling"
let fan_out = "FAN-OUT"
let metrics_names = [ fan_out ]

type context =
  { module_stack : string Stack.t
  ; mutable module_list : string list
  ; module_of_ident : string Ident_Hashtbl.t
  ; paths_in_module : (string, (string, String.comparator_witness) Set.t) Hashtbl.t
  ; metrics_refs : (string, (string, metric_result option ref) Hashtbl.t) Hashtbl.t
  }

let create_metrics_refs () =
  let res =
    Hashtbl.create_mapped
      (module String)
      ~get_key:(fun x -> x)
      ~get_data:(fun _ -> ref None)
      metrics_names
  in
  match res with
  | `Ok x -> x
  | `Duplicate_keys _ -> failwith ""
;;

let ctx =
  { module_stack = Stack.create ()
  ; module_list = []
  ; module_of_ident = Ident_Hashtbl.create 10
  ; paths_in_module = Hashtbl.create (module String)
  ; metrics_refs = Hashtbl.create (module String)
  }
;;

let cur_module () = Stack.top_exn ctx.module_stack

let add_id modname id =
  Hashtbl.update ctx.paths_in_module modname ~f:(fun v ->
      match v with
      | None -> Set.singleton (module String) id
      | Some set -> Set.add set id)
;;

let rec path_name path =
  let open Path in
  match path with
  | Pident id ->
    if Ident.global id
    then Ident.name id
    else (
      match Ident_Hashtbl.find_opt ctx.module_of_ident id with
      | None -> cur_module () ^ "." ^ Ident.name id
      | Some x -> x ^ "." ^ Ident.name id)
  | Pdot (p, s) -> path_name p ^ "." ^ s
  | Papply (p1, _p2) -> path_name p1
;;

let before_module mod_info =
  ctx.module_list <- mod_info.mod_name :: ctx.module_list;
  Stack.push ctx.module_stack mod_info.mod_name;
  (*print_endline mod_info.mod_name;*)
  let _ =
    Hashtbl.add_exn ctx.metrics_refs ~key:mod_info.mod_name ~data:(create_metrics_refs ())
  in
  ()
;;

let get_paths modname =
  match Hashtbl.find ctx.paths_in_module modname with
  | None -> Set.empty (module String)
  | Some x -> x
;;

let calc_fan_out modname =
  let modules =
    Set.filter_map
      (module String)
      (get_paths modname)
      ~f:(fun x ->
        let name = fst @@ String.rsplit2_exn x ~on:'.' in
        if (not (String.equal modname name))
           && List.mem ctx.module_list name ~equal:String.equal
        then Some name
        else None)
  in
  (*print_endline @@ "\n____" ^ modname ^ "____";
  Set.iter modules ~f:print_endline;*)
  Set.length modules
;;

let get_metrics_refs = Hashtbl.find_exn ctx.metrics_refs

let get_module_metrics_result () =
  let this_module = Stack.pop_exn ctx.module_stack in
  let cur_metrics_refs = get_metrics_refs this_module in
  List.map metrics_names ~f:(fun x ->
      x, Delayed_result (Hashtbl.find_exn cur_metrics_refs x))
;;

let collect_delayed_metrics () =
  List.iter ctx.module_list ~f:(fun modname ->
      let cur_metrics_refs = get_metrics_refs modname in
      let get_ref = Hashtbl.find_exn cur_metrics_refs in
      let fan_out_ref = get_ref fan_out in
      fan_out_ref := Some (Int_result (calc_fan_out modname)))
;;

let before_function (func_info : function_info) =
  List.iter func_info.name.name_ident_list ~f:(fun x ->
      Ident_Hashtbl.add ctx.module_of_ident x (cur_module ()))
;;

let get_function_extra_info () = []
let get_function_metrics_result () = []
let get_id_list modname = Set.to_list @@ get_paths modname
let get_module_extra_info () = "Paths in module:" :: get_id_list (cur_module ())

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let open Tast_pattern in
        let pat = map1 (texp_ident __) ~f:path_name in
        Tast_pattern.parse
          pat
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path_name () -> add_id (cur_module ()) path_name)
          ();
        fallback.expr self expr)
  }
;;
