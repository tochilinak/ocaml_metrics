open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type context =
  { mutable res_simple : int
  ; mutable res_rec : int
  ; mutable cur_value_binding : Ident.t option
  }

let ctx : context = { res_simple = 0; res_rec = 0; cur_value_binding = None }
let metric_id = "cyclomatic_complexity"
let extra_info () = []
let reset () = ()

let inner_reset () =
  ctx.res_simple <- 1;
  ctx.res_rec <- 1;
  ctx.cur_value_binding <- None
;;

let get_result () = [ "", float_of_int ctx.res_simple; "_rec", float_of_int ctx.res_rec ]

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
  match expr.exp_desc, ctx.cur_value_binding with
  | Texp_ident (Pident x, _, _), Some y when Ident.equal x y -> 1
  | _ -> 0
;;

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let add = count_add expr in
        ctx.res_simple <- ctx.res_simple + add;
        ctx.res_rec <- ctx.res_rec + add + count_rec expr;
        fallback.expr self expr)
  ; value_binding =
      (fun self vb ->
        let old_vb_name = ctx.cur_value_binding in
        ctx.cur_value_binding
          <- (match vb.vb_pat.pat_desc with
             | Tpat_var (x, _) -> Some x
             | _ -> None);
        fallback.value_binding self vb;
        ctx.cur_value_binding <- old_vb_name)
  }
;;
