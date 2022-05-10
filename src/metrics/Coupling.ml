open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
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
let ca = "CA"
let ce = "CE"
let metrics_names = [ fan_out; fan_in; ca; ce ]

type context =
  { module_stack : string Stack.t
  ; mutable module_list : string list
  ; module_of_ident : string Ident_Hashtbl.t
  ; paths_in_module : (string, (string, String.comparator_witness) Set.t) Hashtbl.t
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

let ctx =
  { module_stack = Stack.create ()
  ; module_list = []
  ; module_of_ident = Ident_Hashtbl.create 10
  ; paths_in_module = Hashtbl.create (module String)
  ; metrics_refs = Hashtbl.create (module String)
  ; filename = ""
  ; module_call_graph = MyDigraph.create ()
  }
;;

let cur_module () = Stack.top_exn ctx.module_stack

let add_id modname id =
  Hashtbl.update ctx.paths_in_module modname ~f:(fun v ->
      match v with
      | None -> Set.singleton (module String) id
      | Some set -> Set.add set id)
;;

let rec path_name path =
  let open Path in
  match path with
  | Pident id ->
    if Ident.global id
    then (
      let name = Ident.name id in
      if String.is_prefix name ~prefix:"Dune__exe"
      then Filename.dirname ctx.filename ^ "|" ^ name
      else name)
    else (
      match Ident_Hashtbl.find_opt ctx.module_of_ident id with
      | None -> cur_module () ^ "." ^ Ident.name id
      | Some x -> x ^ "." ^ Ident.name id)
  | Pdot (p, s) -> path_name p ^ "." ^ s
  | Papply (p1, _p2) -> path_name p1
;;

let before_module mod_info =
  ctx.module_list <- mod_info.mod_name :: ctx.module_list;
  ctx.filename <- mod_info.filename;
  MyDigraph.add_vertex ctx.module_call_graph mod_info.mod_name;
  Stack.push ctx.module_stack mod_info.mod_name;
  (*print_endline mod_info.mod_name;*)
  let _ =
    Hashtbl.add_exn ctx.metrics_refs ~key:mod_info.mod_name ~data:(create_metrics_refs ())
  in
  ()
;;

let get_paths modname =
  match Hashtbl.find ctx.paths_in_module modname with
  | None -> Set.empty (module String)
  | Some x -> x
;;

let add_out_edges modname =
  let get_module func = fst @@ String.rsplit2_exn func ~on:'.' in
  let func_by_modules =
    Set.group_by (get_paths modname) ~equiv:(fun x y ->
        String.equal (get_module x) (get_module y))
  in
  List.iter func_by_modules ~f:(fun set ->
      let x = get_module @@ Set.choose_exn set in
      if (not @@ String.equal modname x) && List.mem ctx.module_list x ~equal:String.equal
      then
        MyDigraph.add_edge_e ctx.module_call_graph
        @@ MyDigraph.E.create modname (Set.length set) x)
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

let get_project_extra_info () =
  [ "Coupling graph:"; Format.asprintf "%a\n" Printer.fprint_graph ctx.module_call_graph ]
;;

let get_metrics_refs = Hashtbl.find_exn ctx.metrics_refs

let get_module_metrics_result () =
  let this_module = Stack.pop_exn ctx.module_stack in
  let cur_metrics_refs = get_metrics_refs this_module in
  List.map metrics_names ~f:(fun x ->
      x, Delayed_result (Hashtbl.find_exn cur_metrics_refs x))
;;

let calc_ca modname =
  MyDigraph.fold_pred_e
    (fun edge acc -> acc + MyDigraph.E.label edge)
    ctx.module_call_graph
    modname
    0
;;

let calc_ce modname =
  MyDigraph.fold_succ_e
    (fun edge acc -> acc + MyDigraph.E.label edge)
    ctx.module_call_graph
    modname
    0
;;

let collect_delayed_metrics () =
  List.iter ctx.module_list ~f:add_out_edges;
  List.iter ctx.module_list ~f:(fun modname ->
      let cur_metrics_refs = get_metrics_refs modname in
      let get_ref = Hashtbl.find_exn cur_metrics_refs in
      let fan_out_ref = get_ref fan_out in
      let fan_in_ref = get_ref fan_in in
      let ca_ref = get_ref ca in
      let ce_ref = get_ref ce in
      fan_out_ref
        := Some (Int_result (MyDigraph.out_degree ctx.module_call_graph modname));
      fan_in_ref := Some (Int_result (MyDigraph.in_degree ctx.module_call_graph modname));
      ca_ref := Some (Int_result (calc_ca modname));
      ce_ref := Some (Int_result (calc_ce modname)))
;;

let before_function (func_info : function_info) =
  List.iter func_info.name.name_ident_list ~f:(fun x ->
      Ident_Hashtbl.add ctx.module_of_ident x (cur_module ()))
;;

let get_function_extra_info () = []
let get_function_metrics_result () = []
let get_id_list modname = Set.to_list @@ get_paths modname
let get_module_extra_info () = "Paths in module:" :: get_id_list (cur_module ())

let run _ _ fallback =
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        let open Tast_pattern in
        let pat = map1 (texp_ident __) ~f:path_name in
        Tast_pattern.parse
          pat
          expr.exp_loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun path_name () ->
            (*print_endline @@ path_name ^ " " ^ (short_location_str expr.exp_loc);*)
            add_id (cur_module ()) path_name)
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
                Ident_Hashtbl.add ctx.module_of_ident var_name (cur_module ()))
              ()
          | _ -> ()
        in
        parse_pat pat;
        fallback.pat self pat)
  }
;;
