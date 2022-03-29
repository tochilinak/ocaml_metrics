open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let metric_id = "experiment"
let result = ref 0
let reset () = result := 0
let update () = result := !result + 1
let get_result () = [ "", float_of_int !result ]
let extra_info () = []

let pat_type_name pat =
  let open Typedtree in
  match pat.pat_desc with
  | Tpat_any -> "Tpat_any"
  | Tpat_var (x, _) -> "Tpat_var " ^ Ident.name x
  | Tpat_alias _ -> "Tpat_alias"
  | Tpat_constant _ -> "Tpat_constant"
  | Tpat_tuple _ -> "Tpat_tuple"
  | Tpat_construct _ -> "Tpat_construct"
  | Tpat_variant _ -> "Tpat_variant"
  | Tpat_record _ -> "Tpat_record"
  | Tpat_array _ -> "Tpat_array"
  | Tpat_lazy _ -> "Tpat_lazy"
  | Tpat_or _ -> "Tpat_or"
;;

let print_structure_type str_desc =
  let open Typedtree in
  match str_desc with
  | Tstr_eval _ -> printfn "eval"
  | Tstr_value (_, x :: _) -> Format.printf "value %s\n" @@ pat_type_name x.vb_pat
  | Tstr_type _ -> printfn "type"
  | _ -> ()
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

let pat_proc : type k. k Typedtree.general_pattern -> unit =
 fun pat ->
  match pat.pat_desc with
  | Tpat_any -> ()
  | Tpat_var _ -> ()
  | Tpat_constant _ -> ()
  | Tpat_tuple _ -> ()
  | Tpat_construct _ -> ()
  | Tpat_variant _ -> ()
  | Tpat_record _ -> ()
  | Tpat_array _ -> ()
  | Tpat_alias _ -> ()
  | Tpat_lazy _ -> ()
  | Tpat_value _ -> print_endline @@ "value " ^ location_str pat.pat_loc
  | Tpat_exception _ -> print_endline @@ "exception " ^ location_str pat.pat_loc
  | Tpat_or _ -> print_endline @@ "or " ^ location_str pat.pat_loc
;;

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    (*pat =
      (fun self pat ->
        pat_proc pat;
        fallback.pat self pat)*)
    (*structure_item =
        (fun self str_item ->
          (*update ();
          print_structure_type str_item.str_desc;
          print_endline @@ loc_printer str_item.str_loc;*)
          fallback.structure_item self str_item);*)
    expr =
      (fun self expr ->
        (match expr.exp_desc with
        | Texp_construct (_, desc, _) when not desc.cstr_loc.loc_ghost ->
          Format.printf "EXP: %s %s\n" desc.cstr_name @@ location_str desc.cstr_loc
        | _ -> ());
        fallback.expr self expr)
      (*; case =
      (fun self case ->
        (*(match case.c_guard with
            | Some e -> print_endline @@ "Some" ^ (loc_printer e.exp_loc)
            | None -> print_endline "None"
          );*)
        fallback.case self case)*)
  }
;;
