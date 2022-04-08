open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

let metric_id = "cyclomatic_complexity"
let extra_info () = []
let result_simple = ref 1
let result_rec = ref 1
let cur_value_binding = ref ""
let reset () = ()

let inner_reset () =
  result_simple := 1;
  result_rec := 1;
  cur_value_binding := ""
;;

let get_result () = [ "", float_of_int !result_simple; "_rec", float_of_int !result_rec ]

let count_add expr =
  let open Typedtree in
  let count_case_add case_list =
    List.fold case_list ~init:0 ~f:(fun acc case ->
        match case.c_guard with
        | None -> acc + 1
        | Some _ -> acc + 2)
  in
  match expr.exp_desc with
  | Texp_ifthenelse _ | Texp_while _ | Texp_for _ -> 1
  | Texp_match (_, cases, _) -> count_case_add cases - 1
  | Texp_function { cases } -> count_case_add cases - 1
  | Texp_try (_, cases) -> count_case_add cases
  | _ -> 0
;;

let count_rec expr =
  let open Typedtree in
  match expr.exp_desc with
  | Texp_ident (x, _, _) when Path.last x == !cur_value_binding -> 1
  | _ -> 0
;;

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let add = count_add expr in
        result_simple := !result_simple + add;
        result_rec := !result_rec + add + count_rec expr;
        fallback.expr self expr)
  ; value_binding =
      (fun self vb ->
        let old_vb_name = !cur_value_binding in
        (cur_value_binding
           := match vb.vb_pat.pat_desc with
              | Tpat_var (x, _) -> Ident.name x
              | _ -> "");
        fallback.value_binding self vb;
        cur_value_binding := old_vb_name)
  }
;;
