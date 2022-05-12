open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC
module Ident_Hashtbl = Caml.Hashtbl.Make (Ident.T)

module MyDigraph =
  Graph.Imperative.Digraph.ConcreteLabeled
    (String)
    (struct
      type t = int

      let compare = Int.compare
      let default = 0
    end)

let metrics_group_id = "coupling"
let fan_out = "FAN-OUT"
let fan_in = "FAN-IN"
let apiu = "APIU"
let metrics_names = [ fan_out; fan_in; apiu ]

type context =
  { module_stack : string Stack.t
  ; mutable module_list : string list
  ; module_of_ident : string Ident_Hashtbl.t
  ; called_functions_in_module :
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t
  ; num_of_struct_func : (string, int) Hashtbl.t
  ; modules_in_module : (string, (string, String.comparator_witness) Set.t) Hashtbl.t
  ; metrics_refs : (string, (string, metric_result option ref) Hashtbl.t) Hashtbl.t
  ; mutable filename : string
  ; module_call_graph : MyDigraph.t
  }

let create_metrics_refs () =
  let res =
    Hashtbl.create_mapped
      (module String)
      ~get_key:(fun x -> x)
      ~get_data:(fun _ -> ref None)
      metrics_names
  in
  match res with
  | `Ok x -> x
  | `Duplicate_keys _ -> failwith ""
;;

let default_ctx () =
  { module_stack = Stack.create ()
  ; module_list = []
  ; module_of_ident = Ident_Hashtbl.create 10
  ; called_functions_in_module = Hashtbl.create (module String)
  ; num_of_struct_func = Hashtbl.create (module String)
  ; modules_in_module = Hashtbl.create (module String)
  ; metrics_refs = Hashtbl.create (module String)
  ; filename = ""
  ; module_call_graph = MyDigraph.create ()
  }
;;

let cur_module ctx () = Stack.top_exn ctx.module_stack

let add_id table modname id =
  Hashtbl.update table modname ~f:(fun v ->
      match v with
      | None -> Set.singleton (module String) id
      | Some set -> Set.add set id)
;;

let add_function ctx = add_id ctx.called_functions_in_module
let add_module ctx = add_id ctx.modules_in_module

let rec path_name ctx path =
  let open Path in
  match path with
  | Pident id ->
    if Ident.global id
    then
      let name = Ident.name id in
      String.chop_prefix_if_exists ~prefix:"Dune__exe"
      @@ String.chop_prefix_if_exists ~prefix:"Dune__exe." name
    else (
      match Ident_Hashtbl.find_opt ctx.module_of_ident id with
      | None -> cur_module ctx () ^ "." ^ Ident.name id
      | Some x -> x ^ "." ^ Ident.name id)
  | Pdot (p, s) ->
      let further = path_name ctx p in
      if String.equal further "" then s else further ^ "." ^ s
  | Papply (p1, _p2) -> path_name ctx p1
;;

let before_module ctx mod_info =
  let _ =
    Hashtbl.add_exn ctx.metrics_refs ~key:mod_info.mod_name ~data:(create_metrics_refs ())
  in
  if mod_info.is_anonymous then add_module ctx (cur_module ctx ()) mod_info.mod_name;
  ctx.module_list <- mod_info.mod_name :: ctx.module_list;
  ctx.filename <- mod_info.filename;
  MyDigraph.add_vertex ctx.module_call_graph mod_info.mod_name;
  Stack.push ctx.module_stack mod_info.mod_name
;;

let get_paths table modname =
  match Hashtbl.find table modname with
  | None -> Set.empty (module String)
  | Some x -> x
;;

let get_function_set ctx = get_paths ctx.called_functions_in_module
let get_module_set ctx = get_paths ctx.modules_in_module

let add_out_edges ctx modname =
  let get_module func = fst @@ String.rsplit2_exn func ~on:'.' in
  let func_by_modules =
    Set.group_by (get_function_set ctx modname) ~equiv:(fun x y ->
        String.equal (get_module x) (get_module y))
  in
  let add_edge dst label =
    if (not @@ String.equal modname dst)
       && List.mem ctx.module_list dst ~equal:String.equal
    then
      MyDigraph.add_edge_e ctx.module_call_graph @@ MyDigraph.E.create modname label dst
  in
  List.iter func_by_modules ~f:(fun set ->
      let x = get_module @@ Set.choose_exn set in
      add_edge x (Set.length set));
  Set.iter (get_module_set ctx modname) ~f:(fun x ->
      if not @@ MyDigraph.mem_edge ctx.module_call_graph modname x then add_edge x 0)
;;

module Printer = Graph.Graphviz.Dot (struct
  include MyDigraph

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes edge = [ `Label (Format.sprintf "%d" @@ E.label edge) ]
  let vertex_name vertex = "\"" ^ vertex ^ "\""
end)

let get_project_extra_info ctx () =
  [ "Coupling graph:"; Format.asprintf "%a\n" Printer.fprint_graph ctx.module_call_graph ]
;;

let get_metrics_refs ctx = Hashtbl.find_exn ctx.metrics_refs

let exit_from_module ctx () =
  let _ = Stack.pop_exn ctx.module_stack in
  ()
;;

let get_module_metrics_result ctx () =
  let cur_metrics_refs = get_metrics_refs ctx (cur_module ctx ()) in
  List.map metrics_names ~f:(fun x ->
      x, Delayed_result (Hashtbl.find_exn cur_metrics_refs x))
;;

let calc_apiu ctx modname =
  let sum, num =
    MyDigraph.fold_pred_e
      (fun edge (s, n) ->
        if MyDigraph.E.label edge > 0 then s + MyDigraph.E.label edge, n + 1 else s, n)
      ctx.module_call_graph
      modname
      (0, 0)
  in
  let func_num =
    match Hashtbl.find ctx.num_of_struct_func modname with
    | None -> 0
    | Some x -> x
  in
  if num * func_num == 0
  then 0.
  else float_of_int sum /. (float_of_int @@ (num * func_num))
;;

let collect_delayed_metrics ctx () =
  List.iter ctx.module_list ~f:(add_out_edges ctx);
  List.iter ctx.module_list ~f:(fun modname ->
      let cur_metrics_refs = get_metrics_refs ctx modname in
      let get_ref = Hashtbl.find_exn cur_metrics_refs in
      let fan_out_ref = get_ref fan_out in
      let fan_in_ref = get_ref fan_in in
      let apiu_ref = get_ref apiu in
      fan_out_ref
        := Some (Int_result (MyDigraph.out_degree ctx.module_call_graph modname));
      fan_in_ref := Some (Int_result (MyDigraph.in_degree ctx.module_call_graph modname));
      apiu_ref := Some (Float_result (calc_apiu ctx modname)))
;;

let before_function ctx (func_info : function_info) =
  let add_struct_func modname =
    Hashtbl.update ctx.num_of_struct_func modname ~f:(function
        | None -> 1
        | Some x -> x + 1)
  in
  add_struct_func (cur_module ctx ());
  List.iter func_info.name.name_ident_list ~f:(fun x ->
      Ident_Hashtbl.add ctx.module_of_ident x (cur_module ctx ()))
;;

let get_id_list ctx modname = Set.to_list @@ get_function_set ctx modname

let get_module_extra_info ctx () =
  "Paths in module:" :: get_id_list ctx (cur_module ctx ())
;;

let run ctx _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let open Tast_pattern in
        let pat = map1 (texp_ident __) ~f:(path_name ctx) in
        Tast_pattern.parse
          pat
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path_name () -> add_function ctx (cur_module ctx ()) path_name)
          ();
        fallback.expr self expr)
  ; pat =
      (fun self pat ->
        let parse_pat : type k. k Tast_pattern.gen_pat -> unit =
         fun pat ->
          let open Tast_pattern in
          let parse_pat = tpat_var_ident __ in
          match Tast_pattern.convert_gen_pat pat with
          | Value x ->
            Tast_pattern.parse
              parse_pat
              pat.pat_loc
              ~on_error:(fun _desc () -> ())
              x
              (fun var_name () ->
                Ident_Hashtbl.add ctx.module_of_ident var_name (cur_module ctx ()))
              ()
          | _ -> ()
        in
        parse_pat pat;
        fallback.pat self pat)
  ; module_expr =
      (fun self mod_expr ->
        let open Tast_pattern in
        let pat = map1 (tmod_ident __) ~f:(path_name ctx) in
        Tast_pattern.parse
          pat
          mod_expr.mod_loc
          ~on_error:(fun _desc () -> ())
          mod_expr
          (fun modname () -> add_module ctx (cur_module ctx ()) modname)
          ();
        fallback.module_expr self mod_expr)
  }
;;

let get_iterators () =
  let ctx = default_ctx () in
  let cmt_iterator =
    { actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = before_function ctx
        ; begin_of_module = before_module ctx
        ; end_of_module =
            (fun _ ->
              let result = get_module_metrics_result ctx () in
              let extra_info = get_module_extra_info ctx () in
              exit_from_module ctx ();
              result, extra_info)
        }
    ; run = run ctx
    ; collect_delayed_metrics = collect_delayed_metrics ctx
    ; get_project_extra_info = get_project_extra_info ctx
    }
  in
  cmt_iterator, default_group_iterator
;;
