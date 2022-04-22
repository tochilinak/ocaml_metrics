open Base
module Format = Caml.Format
module Hashtbl = Caml.Hashtbl
open Zanuda_core
open Zanuda_core.Utils

module MyGraph = Graph.Imperative.Graph.Concrete(Ident);;

type context =
  { mutable num_of_methods : int
  ; mutable possible_arcs : int
  ; mutable cur_function : Ident.t option
  ; mutable graph : MyGraph.t
  }

module Builder = Graph.Builder.I(MyGraph)

let ctx : context =
  { num_of_methods = 0
  ; possible_arcs = 0
  ; cur_function = None
  ; graph = Builder.empty ()
  }
;;

let metrics_group_id = "cohesion"
let get_function_metrics_result () = []
let get_function_extra_info () = []

let reset () =
  ctx.num_of_methods <- 0;
  ctx.possible_arcs <- 0;
  ctx.cur_function <- None;
  ctx.graph <- Builder.empty ()
;;

let before_function func_info =
  ctx.cur_function <- func_info.name;
  match func_info.name with
  | None -> ()
  | Some x ->
    let loop = if func_info.is_rec then 1 else 0 in
    ctx.possible_arcs <- ctx.possible_arcs + loop + ctx.num_of_methods;
    ctx.num_of_methods <- ctx.num_of_methods + 1;
    ctx.graph <- Builder.add_vertex ctx.graph x
;;

let get_module_extra_info () =
  "COHESION GRAPH:" ::
  MyGraph.fold_edges
    (fun u v acc -> (Format.sprintf "%s -> %s" (Ident.name u) (Ident.name v)) :: acc)
    ctx.graph
    []
;;

module Components = Graph.Components.Undirected(MyGraph)

let get_module_metrics_result () =
  [ "LCOM34", Int_result (fst @@ Components.components ctx.graph)]
;;

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let look_for_edge cur_func =
          let pat =
            let open Tast_pattern in
            let cur_pident =
              let one_pident ident =
                path_pident @@ cst ~to_string:Ident.name ~equal:Ident.equal ident
              in
              MyGraph.fold_vertex
                (fun func acc -> acc ||| map0 (one_pident func) ~f:func)
                ctx.graph
                reject
            in
            texp_ident cur_pident
          in
          Tast_pattern.parse
            pat
            expr.exp_loc
            ~on_error:(fun _desc () -> ())
            expr
            (fun func () -> ctx.graph <- Builder.add_edge ctx.graph cur_func func)
          ()
        in
        (match ctx.cur_function with
        | None -> ()
        | Some x -> look_for_edge x);
        fallback.expr self expr)
  }
;;
