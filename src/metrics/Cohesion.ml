open Base
module Format = Caml.Format
module Hashtbl = Caml.Hashtbl
open Zanuda_core
open Zanuda_core.Utils

type context =
  { mutable num_of_methods : int
  ; mutable possible_arcs : int
  ; mutable cur_function : Ident.t option
  ; func_id : (Ident.t, int) Hashtbl.t
  ; func_by_id : (int, Ident.t) Hashtbl.t
  ; mutable edge_list : (int * int) list
  }

let ctx : context =
  { num_of_methods = 0
  ; possible_arcs = 0
  ; cur_function = None
  ; func_id = Hashtbl.create 10
  ; func_by_id = Hashtbl.create 10
  ; edge_list = []
  }
;;

let metrics_group_id = "cohesion"
let get_function_metrics_result () = []
let get_function_extra_info () = []

let reset () =
  ctx.num_of_methods <- 0;
  ctx.possible_arcs <- 0;
  ctx.cur_function <- None;
  Hashtbl.clear ctx.func_id;
  Hashtbl.clear ctx.func_by_id;
  ctx.edge_list <- []
;;

let before_function func_info =
  ctx.cur_function <- func_info.name;
  match func_info.name with
  | None -> ()
  | Some x ->
    let loop = if func_info.is_rec then 1 else 0 in
    ctx.possible_arcs <- ctx.possible_arcs + loop + ctx.num_of_methods;
    ctx.num_of_methods <- ctx.num_of_methods + 1;
    Hashtbl.add ctx.func_id x (ctx.num_of_methods - 1);
    Hashtbl.add ctx.func_by_id (ctx.num_of_methods - 1) x
;;

let get_module_extra_info () =
  let get_name v = Ident.name @@ Hashtbl.find ctx.func_by_id v in
  "COHESION GRAPH:"
  :: List.map ctx.edge_list ~f:(fun (u, v) ->
         Format.sprintf "%s -> %s" (get_name u) (get_name v))
;;

let get_module_metrics_result () =
  let g = Graph.init_graph ctx.num_of_methods in
  List.iter ctx.edge_list ~f:(fun (u, v) -> Graph.add_edge g u v);
  [ "LCOM34", Int_result (Graph.count_comp g) ]
;;

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let look_for_edge =
          let pat =
            let open Tast_pattern in
            let cur_pident =
              let one_pident ident =
                path_pident @@ cst ~to_string:Ident.name ~equal:Ident.equal ident
              in
              Hashtbl.fold
                (fun key id acc -> acc ||| map0 (one_pident key) ~f:id)
                ctx.func_id
                reject
            in
            texp_ident cur_pident
          in
          let cur_id = ctx.num_of_methods - 1 in
          Tast_pattern.parse
            pat
            expr.exp_loc
            ~on_error:(fun _desc () -> ())
            expr
            (fun id () -> ctx.edge_list <- (cur_id, id) :: ctx.edge_list)
        in
        (match ctx.cur_function with
        | None -> ()
        | Some _ -> look_for_edge ());
        fallback.expr self expr)
  }
;;
