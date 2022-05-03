open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

let metrics_group_id = "coupling"

type context =
  { mutable cur_module : string
  ; idents_in_module : (string, (string, String.comparator_witness) Set.t) Hashtbl.t
  }

let ctx = { cur_module = ""; idents_in_module = Hashtbl.create (module String) }

let add_id modname id =
  Hashtbl.update ctx.idents_in_module modname ~f:(fun v ->
      match v with
      | None -> Set.singleton (module String) id
      | Some set -> Set.add set id)
;;

(*
let rec path_full_name path =
  let open Path in
  match path with
  | Pident id ->
    if Ident.global id then Ident.name id else ctx.cur_module ^ "." ^ Ident.name id
  | Pdot (p, s) -> path_full_name p ^ "." ^ s
  | Papply (p1, p2) -> path_full_name p1 ^ "(" ^ path_full_name p2 ^ ")"
;;
*)

let before_module mod_info = ctx.cur_module <- mod_info.mod_name
let before_function _ = ()
let get_function_extra_info () = []
let get_module_metrics_result () = []
let get_function_metrics_result () = []

let get_id_list modname =
  match Hashtbl.find ctx.idents_in_module modname with
  | None -> []
  | Some set -> Set.to_list set
;;

let get_module_extra_info () = "Paths in module:" :: get_id_list ctx.cur_module

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let open Tast_pattern in
        let pat = map1 (texp_ident __) ~f:Path.name in
        Tast_pattern.parse
          pat
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path_name () -> add_id ctx.cur_module path_name)
          ();
        fallback.expr self expr)
  }
;;
