open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC

type context =
  { mutable res_simple : int
  ; mutable res_rec : int
  ; mutable res_mod : int
  ; mutable cur_value_binding : Ident.t option
  ; mutable is_rec : bool
  ; mutable let_is_rec : bool
  ; mutable max_ord : int
  ; mutable sum_ord : int
  ; mutable func_num : int
  }

let get_ctx contexts = Stack.top_exn contexts

let before_module contexts _ =
  Stack.push
    contexts
    { res_simple = 0
    ; res_rec = 0
    ; res_mod = 0
    ; cur_value_binding = None
    ; is_rec = false
    ; let_is_rec = false
    ; max_ord = 0
    ; sum_ord = 0
    ; func_num = 0
    }
;;

let metrics_group_id = "CC-based"

let get_module_metrics_result contexts _ =
  let ctx = get_ctx contexts in
  let f = float_of_int in
  [ "mod-ord-max", Int_result ctx.max_ord
  ; "mod-ord-avg", Float_result (f ctx.sum_ord /. f ctx.func_num)
  ]
;;

let before_function contexts (func_info : function_info) =
  let ctx = get_ctx contexts in
  ctx.res_simple <- 1;
  ctx.res_rec <- 1;
  ctx.res_mod <- 1;
  ctx.cur_value_binding <- None;
  ctx.let_is_rec <- func_info.is_rec
;;

let get_function_metrics_result contexts () =
  let ctx = get_ctx contexts in
  ctx.max_ord <- max ctx.max_ord ctx.res_simple;
  ctx.sum_ord <- ctx.sum_ord + ctx.res_simple;
  ctx.func_num <- ctx.func_num + 1;
  [ "ord", Int_result ctx.res_simple
  ; "rec", Int_result ctx.res_rec
  ; "mod", Int_result ctx.res_mod
  ]
;;

let common_pat =
  let open Tast_pattern in
  map0
    (texp_ite drop drop drop
    ||| texp_while
    ||| texp_for
    ||| texp_ident (path [ "Stdlib"; "&&" ])
    ||| texp_ident (path [ "Stdlib"; "||" ])
    ||| texp_ident (path [ "Base"; "&&" ])
    ||| texp_ident (path [ "Base"; "||" ]))
    ~f:1
;;

let count_case_add case_list case_add =
  let open Typedtree in
  List.fold case_list ~init:0 ~f:(fun acc case ->
      match case.c_guard with
      | None -> acc + case_add
      | Some _ -> acc + case_add + 1)
;;

let pat_ord =
  let open Tast_pattern in
  map1 (texp_match drop __) ~f:(fun x -> count_case_add x 1 - 1)
  ||| map1 (texp_function __) ~f:(fun x -> count_case_add x 1 - 1)
  ||| map1 (texp_try drop __) ~f:(fun x -> count_case_add x 1)
  ||| common_pat
;;

let pat_mod =
  let open Tast_pattern in
  let count_case_add_mod cases = if List.length cases > 1 then 1 else 0 in
  map1 (texp_match drop __) ~f:(fun x -> count_case_add x 0 + count_case_add_mod x)
  ||| map1 (texp_function __) ~f:(fun x -> count_case_add x 0 + count_case_add_mod x)
  ||| map1 (texp_try drop __) ~f:(fun x -> count_case_add x 1)
  ||| common_pat
;;

let count_add pat expr =
  Tast_pattern.parse
    pat
    expr.Typedtree.exp_loc
    ~on_error:(fun _desc () -> 0)
    expr
    (fun x () -> x)
    ()
;;

let count_rec contexts expr =
  let ctx = get_ctx contexts in
  let open Typedtree in
  match expr.exp_desc, ctx.cur_value_binding with
  | Texp_ident (Pident x, _, _), Some y when Ident.T.equal x y -> 1
  | _ -> 0
;;

let change_let_is_rec contexts expr =
  let ctx = get_ctx contexts in
  let open Typedtree in
  match expr.exp_desc with
  | Texp_let (x, _, _) -> ctx.let_is_rec <- rec_flag_to_bool x
  | _ -> ()
;;

let run contexts _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let ctx = get_ctx contexts in
        let old_let_is_rec = ctx.let_is_rec in
        change_let_is_rec contexts expr;
        let add_ord = count_add pat_ord expr in
        let rec_add = if ctx.is_rec then count_rec contexts expr else 0 in
        ctx.res_simple <- ctx.res_simple + add_ord;
        ctx.res_rec <- ctx.res_rec + add_ord + rec_add;
        ctx.res_mod <- ctx.res_mod + count_add pat_mod expr;
        fallback.expr self expr;
        ctx.let_is_rec <- old_let_is_rec)
  ; value_binding =
      (fun self vb ->
        let ctx = get_ctx contexts in
        let old_vb_name = ctx.cur_value_binding in
        let old_is_rec = ctx.is_rec in
        ctx.cur_value_binding <- get_vb_name_ident vb;
        ctx.is_rec <- ctx.let_is_rec;
        fallback.value_binding self vb;
        ctx.is_rec <- old_is_rec;
        ctx.cur_value_binding <- old_vb_name)
  }
;;

let get_iterator_builder () =
  let contexts = Stack.create () in
  let cmt_iterator =
    { actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = before_function contexts
        ; end_of_function = (fun _ -> get_function_metrics_result contexts (), [])
        ; begin_of_module = before_module contexts
        ; end_of_module =
            (fun _ ->
              let res = get_module_metrics_result contexts () in
              let _ = Stack.pop contexts in
              res, [])
        }
    ; run = run contexts
    }
  in
  { default_metrics_group_iterator_builder with cmt = cmt_iterator }
;;
