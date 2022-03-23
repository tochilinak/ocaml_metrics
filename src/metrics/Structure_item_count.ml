open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let metric_id = "structure_item_count"
let result = ref 0
let reset () = result := 0
let update () = result := !result + 1
let get_result () = [ "", float_of_int !result ]

let print_structure_type str_desc =
  let open Typedtree in
  match str_desc with
  | Tstr_eval _ -> printfn "eval"
  | Tstr_value _ -> printfn "value"
  | Tstr_type _ -> printfn "type"
  | _ -> ()
;;

let loc_printer loc =
  Location.print_loc Format.str_formatter loc;
  String.drop_prefix (String.drop_suffix (Format.flush_str_formatter ()) 4) 4
;;

let print_empty_construct_name expr =
  let open Typedtree in
  let open Types in
  let open Location in
  let print_constr x =
    print_endline @@ "construct: " ^ x.cstr_name;
    match x.cstr_loc.loc_ghost with
    | true -> print_endline "true"
    | false -> print_endline "false"
  in
  let open Typedtree in
  match expr with
  | Texp_construct (_, x, []) -> print_constr x
  | _ -> ()
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    (*structure_item =
        (fun self str_item ->
          update ();
          print_structure_type str_item.str_desc;
          fallback.structure_item self str_item)*)
    expr =
      (fun self expr ->
        (*print_endline "-------------------------";
          print_endline @@ Texp_names.texp_name expr.exp_desc;
          print_endline @@ loc_printer expr.exp_loc;
          print_empty_construct_name expr.exp_desc;
          print_endline "-------------------------";*)
        fallback.expr self expr)
  ; case =
      (fun self case ->
        (*(match case.c_guard with
            | Some e -> print_endline @@ "Some" ^ (loc_printer e.exp_loc)
            | None -> print_endline "None"
          );*)
        fallback.case self case)
  }
;;
