open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC
module MyGraph = Graph.Imperative.Graph.Concrete (String)
module MyDigraph = Graph.Imperative.Digraph.Concrete (String)
module Hashtbl = Caml.Hashtbl.Make (Ident.T)

let metrics_group_id = "cohesion"

type context =
  { mutable num_of_methods : int
  ; mutable possible_arcs : int
  ; mutable cur_function : string
  ; mutable idents : Ident.t list
  ; ident_to_vertex : string Hashtbl.t
  ; digraph : MyDigraph.t
  ; graph : MyGraph.t
  }

let get_ctx contexts = Stack.top_exn contexts

let before_module contexts _ =
  Stack.push
    contexts
    { num_of_methods = 0
    ; possible_arcs = 0
    ; cur_function = ""
    ; idents = []
    ; ident_to_vertex = Hashtbl.create 10
    ; digraph = MyDigraph.create ()
    ; graph = MyGraph.create ()
    }
;;

let before_function contexts (func_info : function_info) =
  let ctx = get_ctx contexts in
  ctx.cur_function <- func_info.name.name_string;
  let outside_block = ctx.num_of_methods - func_info.ind_inside_block in
  let inside_block = if func_info.is_rec then List.length func_info.block else 0 in
  ctx.possible_arcs <- ctx.possible_arcs + outside_block + inside_block;
  ctx.num_of_methods <- ctx.num_of_methods + 1;
  if func_info.ind_inside_block == 0
  then
    List.iter func_info.block ~f:(fun { name_ident_list; name_string } ->
        ctx.idents <- name_ident_list @ ctx.idents;
        List.iter name_ident_list ~f:(fun x ->
            Hashtbl.add ctx.ident_to_vertex x name_string);
        MyDigraph.add_vertex ctx.digraph name_string;
        MyGraph.add_vertex ctx.graph name_string)
;;

module Printer = Graph.Graphviz.Dot (struct
  include MyDigraph

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let vertex_name vertex = "\"" ^ vertex ^ "\""
end)

let get_module_extra_info contexts () =
  let ctx = get_ctx contexts in
  [ Format.sprintf "Maximum possible arcs: %d" ctx.possible_arcs
  ; "COHESION GRAPH:"
  ; Format.asprintf "%a\n" Printer.fprint_graph ctx.digraph
  ]
;;

let calc_lcom1 contexts () =
  let ctx = get_ctx contexts in
  let int_of_bool b = if b then 1 else 0 in
  let func_list = MyDigraph.fold_vertex (fun v acc -> v :: acc) ctx.digraph [] in
  let ind_func_list = List.zip_exn (List.mapi func_list ~f:(fun i _ -> i)) func_list in
  let func_pairs = List.cartesian_product ind_func_list ind_func_list in
  List.fold func_pairs ~init:0 ~f:(fun acc ((i1, func1), (i2, func2)) ->
      if i1 >= i2
      then acc
      else (
        let d1 = MyDigraph.out_degree ctx.digraph func1 in
        let d2 = MyDigraph.out_degree ctx.digraph func2 in
        let u1, u2 = if d1 < d2 then func1, func2 else func2, func1 in
        if MyGraph.mem_edge ctx.graph u1 u2
        then acc
        else
          acc
          + int_of_bool
              (MyDigraph.fold_succ
                 (fun v found -> found && (not @@ MyDigraph.mem_edge ctx.digraph u2 v))
                 ctx.digraph
                 u1
                 true)))
;;

module Components = Graph.Components.Undirected (MyGraph)

let calc_result contexts () =
  let ctx = get_ctx contexts in
  let lcom1 = calc_lcom1 contexts () in
  let lcom2 =
    max 0 @@ ((2 * lcom1) - (ctx.num_of_methods * (ctx.num_of_methods - 1) / 2))
  in
  let f = float_of_int in
  let a = MyDigraph.nb_edges ctx.digraph in
  let e = ctx.possible_arcs in
  let l = ctx.num_of_methods in
  let lcom5 =
    let down = l - e in
    if down == 0 then 0. else (f @@ (a - e)) /. f down
  in
  let coh = if e == 0 then 1. else f a /. f e in
  [ "COH", Float_result coh
  ; "LCOM1", Int_result lcom1
  ; "LCOM2", Int_result lcom2
  ; "LCOM34", Int_result (fst @@ Components.components ctx.graph)
  ; "LCOM5", Float_result lcom5
  ]
;;

let get_module_metrics_result contexts () =
  let result = calc_result contexts () in
  let _ = Stack.pop_exn contexts in
  result
;;

let run contexts _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let ctx = get_ctx contexts in
        let look_for_edge cur_func =
          let pat =
            let open Tast_pattern in
            let cur_pident =
              let one_pident ident =
                path_pident @@ cst ~to_string:Ident.name ~equal:Ident.T.equal ident
              in
              List.fold
                ctx.idents
                ~f:(fun acc id ->
                  acc ||| map0 (one_pident id) ~f:(Hashtbl.find ctx.ident_to_vertex id))
                ~init:reject
            in
            texp_ident cur_pident
          in
          Tast_pattern.parse
            pat
            expr.exp_loc
            ~on_error:(fun _desc () -> ())
            expr
            (fun func () ->
              MyDigraph.add_edge ctx.digraph cur_func func;
              MyGraph.add_edge ctx.graph cur_func func)
            ()
        in
        if not @@ String.equal ctx.cur_function "" then look_for_edge ctx.cur_function;
        fallback.expr self expr)
  }
;;

let get_iterator_builder () =
  let contexts : context Stack.t = Stack.create () in
  let cmt_iterator =
    { run = run contexts
    ; actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = before_function contexts
        ; begin_of_module = before_module contexts
        ; end_of_module =
            (fun _ ->
              get_module_metrics_result contexts (), get_module_extra_info contexts ())
        }
    }
  in
  { default_metrics_group_iterator_builder with cmt = cmt_iterator }
;;
