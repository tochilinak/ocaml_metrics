open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

let metric_id = "cyclomatic_complexity"
let extra_info () = []
let result = ref 1
let reset () = ()
let inner_reset () = result := 1
let get_result () = [ "", float_of_int !result ]

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

let run _ _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        result := !result + count_add expr;
        fallback.expr self expr)
  }
;;
