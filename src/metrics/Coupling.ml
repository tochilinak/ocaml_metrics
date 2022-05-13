open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC
module Ident_Hashtbl = Caml.Hashtbl.Make (Ident.T)

module Edge_label = struct
  type t =
    { functions_used : int
    ; consider_in_apiu : bool
    }

  let compare e1 e2 = Int.compare e1.functions_used e2.functions_used
  let default = { functions_used = 0; consider_in_apiu = false }
end

module MyDigraph = Graph.Imperative.Digraph.ConcreteLabeled (String) (Edge_label)

let metrics_group_id = "coupling"
let fan_out = "FAN-OUT"
let fan_in = "FAN-IN"
let apiu = "APIU"
let metrics_names = [ fan_out; fan_in; apiu ]

type context =
  { mutable cur_executable : string option
  ; module_stack : (string * bool) Stack.t
  ; mutable cur_function : string
  ; module_of_ident : string Ident_Hashtbl.t
  ; mutable struct_modules :
      (string, String.comparator_witness) Set.t (* modules from .ml *)
  ; mutable sig_modules :
      (string, String.comparator_witness) Set.t (* modules from .mli *)
  ; struct_functions_in_module :
      (* functions from .ml *)
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t (* key: modname *)
  ; sig_functions_in_module :
      (* functions from .mli *)
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t (* key: modname *)
  ; api_functions_in_module :
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t (* key: modname *)
  ; called_items_from_function :
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t
        (* key: modname.func_name; data: modname.func_name*)
  ; refered_modules_in_module :
      (string, (string, String.comparator_witness) Set.t) Hashtbl.t (* key: modname *)
  ; file_of_module : (string, string) Hashtbl.t (* filenames without extension *)
  ; mutable ml_files :
      (string, String.comparator_witness) Set.t (* filenames without extension *)
  ; mutable mli_files :
      (string, String.comparator_witness) Set.t (* filenames without extension *)
  ; metrics_refs :
      (string, (string, metric_result option ref) Hashtbl.t) Hashtbl.t (* key: modname *)
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
  { cur_executable = None
  ; module_stack = Stack.create ()
  ; cur_function = ""
  ; module_of_ident = Ident_Hashtbl.create 10
  ; struct_modules = Set.empty (module String)
  ; sig_modules = Set.empty (module String)
  ; struct_functions_in_module = Hashtbl.create (module String)
  ; sig_functions_in_module = Hashtbl.create (module String)
  ; api_functions_in_module = Hashtbl.create (module String)
  ; called_items_from_function = Hashtbl.create (module String)
  ; refered_modules_in_module = Hashtbl.create (module String)
  ; file_of_module = Hashtbl.create (module String)
  ; ml_files = Set.empty (module String)
  ; mli_files = Set.empty (module String)
  ; metrics_refs = Hashtbl.create (module String)
  ; module_call_graph = MyDigraph.create ()
  }
;;

let cur_module ctx = fst @@ Stack.top_exn ctx.module_stack
let cur_module_is_anonymous ctx = snd @@ Stack.top_exn ctx.module_stack

let rec path_name ctx path =
  let open Path in
  match path with
  | Pident id ->
    if Ident.global id
    then (
      let name = Ident.name id in
      if String.is_prefix ~prefix:"Dune__exe" name
      then (
        let exe_name =
          match ctx.cur_executable with
          | None -> assert false
          | Some x -> x
        in
        exe_name ^ String.chop_prefix_exn ~prefix:"Dune__exe" name)
      else name)
    else (
      match Ident_Hashtbl.find_opt ctx.module_of_ident id with
      | None -> cur_module ctx ^ "." ^ Ident.name id
      | Some x -> x ^ "." ^ Ident.name id)
  | Pdot (p, s) ->
    let further = path_name ctx p in
    if String.equal further "" then s else further ^ "." ^ s
  | Papply (p1, _p2) -> path_name ctx p1
;;

let add_item table key item =
  Hashtbl.update table key ~f:(fun v ->
      match v with
      | None -> Set.singleton (module String) item
      | Some set -> Set.add set item)
;;

let default_find table key =
  match Hashtbl.find table key with
  | None -> Set.empty (module String)
  | Some x -> x
;;

let begin_of_cmt_module ctx mod_info =
  let is_anonymous =
    mod_info.is_anonymous
    || ((not (Stack.is_empty ctx.module_stack)) && cur_module_is_anonymous ctx)
  in
  Stack.push ctx.module_stack (mod_info.mod_name, is_anonymous);
  if not is_anonymous
  then (
    ctx.struct_modules <- Set.add ctx.struct_modules mod_info.mod_name;
    let filename = String.chop_suffix_exn mod_info.filename ~suffix:".ml" in
    Hashtbl.add_exn ctx.file_of_module ~key:mod_info.mod_name ~data:filename;
    ctx.ml_files <- Set.add ctx.ml_files filename;
    Hashtbl.add_exn ctx.metrics_refs ~key:mod_info.mod_name ~data:(create_metrics_refs ()))
;;

let begin_of_cmti_module ctx mod_sig_info =
  Stack.push ctx.module_stack (mod_sig_info.mod_sig_name, false);
  ctx.sig_modules <- Set.add ctx.sig_modules mod_sig_info.mod_sig_name;
  let filename = String.chop_suffix_exn mod_sig_info.filename ~suffix:".mli" in
  ctx.mli_files <- Set.add ctx.mli_files filename
;;

let exit_from_module ctx () =
  let _ = Stack.pop_exn ctx.module_stack in
  ()
;;

let begin_of_cmt_function ctx (func_info : function_info) =
  if not @@ cur_module_is_anonymous ctx
  then (
    ctx.cur_function <- func_info.name.name_string;
    add_item ctx.struct_functions_in_module (cur_module ctx) ctx.cur_function;
    List.iter func_info.name.name_ident_list ~f:(fun x ->
        add_item ctx.api_functions_in_module (cur_module ctx) (Ident.name x)))
;;

let end_of_function ctx _ = ctx.cur_function <- ""

let begin_of_cmti_function ctx func_sig_info =
  ctx.cur_function <- Ident.name func_sig_info.fun_sig_name;
  add_item ctx.sig_functions_in_module (cur_module ctx) ctx.cur_function
;;

let get_vertex_modules ctx () =
  Set.fold ctx.struct_modules ~init:[] ~f:(fun acc cur_mod ->
      let file_of_mod = Hashtbl.find_exn ctx.file_of_module cur_mod in
      if not @@ Set.mem ctx.mli_files file_of_mod
      then (
        if not @@ Hashtbl.mem ctx.api_functions_in_module cur_mod
        then
          Hashtbl.add_exn
            ctx.api_functions_in_module
            ~key:cur_mod
            ~data:(Set.empty (module String));
        cur_mod :: acc)
      else if Set.mem ctx.sig_modules cur_mod
      then (
        Hashtbl.remove ctx.api_functions_in_module cur_mod;
        Hashtbl.add_exn
          ctx.api_functions_in_module
          ~key:cur_mod
          ~data:(default_find ctx.sig_functions_in_module cur_mod);
        cur_mod :: acc)
      else acc)
;;

let add_out_edges ctx modname =
  let add_edge dst label =
    MyDigraph.add_edge_e ctx.module_call_graph @@ MyDigraph.E.create modname label dst
  in
  let calls_to_module_list calls =
    Set.fold calls ~init:[] ~f:(fun acc func_with_path ->
        let mod_of_func, func = String.rsplit2_exn ~on:'.' func_with_path in
        if String.equal modname mod_of_func
           || (not @@ MyDigraph.mem_vertex ctx.module_call_graph mod_of_func)
        then acc
        else if not @@ Set.mem (default_find ctx.api_functions_in_module mod_of_func) func
        then (* non-api function call *)
          (mod_of_func, func, true) :: acc
        else (mod_of_func, func, false) :: acc)
  in
  let module_function_set : (string, (string, String.comparator_witness) Set.t) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let module_counter : (string, Edge_label.t) Hashtbl.t =
    Hashtbl.create (module String)
  in
  Set.iter (default_find ctx.struct_functions_in_module modname) ~f:(fun func ->
      let func_with_mod = modname ^ "." ^ func in
      let calls = default_find ctx.called_items_from_function func_with_mod in
      let module_list = calls_to_module_list calls in
      List.iter module_list ~f:(fun (cur_mod, cur_func, is_non_api_call) ->
          let add =
            if Set.mem (default_find module_function_set cur_mod) cur_func then 0 else 1
          in
          add_item module_function_set cur_mod cur_func;
          Hashtbl.update module_counter cur_mod ~f:(function
              | None -> { functions_used = 1; consider_in_apiu = not is_non_api_call }
              | Some { Edge_label.functions_used; consider_in_apiu } ->
                { functions_used = functions_used + add
                ; consider_in_apiu = consider_in_apiu && not is_non_api_call
                })));
  Hashtbl.iteri module_counter ~f:(fun ~key ~data -> add_edge key data);
  Set.iter (default_find ctx.refered_modules_in_module modname) ~f:(fun cur_mod ->
      if MyDigraph.mem_vertex ctx.module_call_graph cur_mod
         && (not @@ MyDigraph.mem_edge ctx.module_call_graph modname cur_mod)
      then add_edge cur_mod Edge_label.default)
;;

let build_graph ctx () =
  let vertex_modules = get_vertex_modules ctx () in
  List.iter vertex_modules ~f:(MyDigraph.add_vertex ctx.module_call_graph);
  List.iter vertex_modules ~f:(add_out_edges ctx)
;;

let get_metrics_refs ctx = Hashtbl.find_exn ctx.metrics_refs

let get_module_metrics_result_refs ctx () =
  if cur_module_is_anonymous ctx
  then []
  else (
    let cur_metrics_refs = get_metrics_refs ctx (cur_module ctx) in
    List.map metrics_names ~f:(fun x ->
        x, Delayed_result (Hashtbl.find_exn cur_metrics_refs x)))
;;

let calc_apiu ctx modname =
  let sum, num =
    MyDigraph.fold_pred_e
      (fun edge (s, n) ->
        if (MyDigraph.E.label edge).consider_in_apiu
        then s + (MyDigraph.E.label edge).Edge_label.functions_used, n + 1
        else s, n)
      ctx.module_call_graph
      modname
      (0, 0)
  in
  let func_num = Set.length @@ Hashtbl.find_exn ctx.api_functions_in_module modname in
  if num * func_num == 0
  then 0.
  else float_of_int sum /. (float_of_int @@ (num * func_num))
;;

let collect_delayed_metrics ctx () =
  build_graph ctx ();
  MyDigraph.iter_vertex
    (fun modname ->
      let cur_metrics_refs = get_metrics_refs ctx modname in
      let get_ref = Hashtbl.find_exn cur_metrics_refs in
      let fan_out_ref = get_ref fan_out in
      let fan_in_ref = get_ref fan_in in
      let apiu_ref = get_ref apiu in
      fan_out_ref
        := Some (Int_result (MyDigraph.out_degree ctx.module_call_graph modname));
      fan_in_ref := Some (Int_result (MyDigraph.in_degree ctx.module_call_graph modname));
      apiu_ref := Some (Float_result (calc_apiu ctx modname)))
    ctx.module_call_graph
;;

module Printer = Graph.Graphviz.Dot (struct
  include MyDigraph

  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let get_subgraph _ = None
  let default_edge_attributes _ = []

  let edge_attributes edge =
    [ `Label (Format.sprintf "%d" @@ (E.label edge).functions_used) ]
  ;;

  let vertex_name vertex = "\"" ^ vertex ^ "\""
end)

let get_function_extra_info ctx () =
  "Called from function:"
  :: (Set.to_list
     @@ default_find
          ctx.called_items_from_function
          (cur_module ctx ^ "." ^ ctx.cur_function))
;;

let get_project_extra_info ctx () =
  [ "Coupling graph:"; Format.asprintf "%a\n" Printer.fprint_graph ctx.module_call_graph ]
;;

(*@ [ "API functions:" ]
  @ Hashtbl.fold ctx.api_functions_in_module ~init:[] ~f:(fun ~key ~data acc ->
        (Format.sprintf "Module %s" key
        :: Set.fold data ~init:[] ~f:(fun acc x -> x :: acc))
        @ [ "\n" ]
        @ acc)*)

let run_cmt ctx (_, exe_name) _ fallback =
  ctx.cur_executable <- exe_name;
  let open Tast_iterator in
  let open Typedtree in
  { fallback with
    expr =
      (fun self expr ->
        (if (not @@ cur_module_is_anonymous ctx) && not expr.exp_loc.loc_ghost
        then
          let open Tast_pattern in
          let pat = map1 (texp_ident __) ~f:(path_name ctx) in
          Tast_pattern.parse
            pat
            expr.exp_loc
            ~on_error:(fun _desc () -> ())
            expr
            (fun func_with_path () ->
              let cur_func_with_mod = cur_module ctx ^ "." ^ ctx.cur_function in
              add_item ctx.called_items_from_function cur_func_with_mod func_with_path)
            ());
        fallback.expr self expr)
  ; pat =
      (fun self pat ->
        if (not @@ cur_module_is_anonymous ctx) && not pat.pat_loc.loc_ghost
        then (
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
                  Ident_Hashtbl.add ctx.module_of_ident var_name (cur_module ctx))
                ()
            | _ -> ()
          in
          parse_pat pat);
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
          (fun modname () ->
            add_item ctx.refered_modules_in_module (cur_module ctx) modname)
          ();
        fallback.module_expr self mod_expr)
  }
;;

let get_iterators () =
  let ctx = default_ctx () in
  let cmt_iterator =
    { actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = begin_of_cmt_function ctx
        ; end_of_function =
            (fun _ ->
              let extra_info = get_function_extra_info ctx () in
              end_of_function ctx ();
              [], extra_info)
        ; begin_of_module = begin_of_cmt_module ctx
        ; end_of_module =
            (fun _ ->
              let result_refs = get_module_metrics_result_refs ctx () in
              exit_from_module ctx ();
              result_refs, [])
        }
    ; run = run_cmt ctx
    ; collect_delayed_metrics = collect_delayed_metrics ctx
    ; get_project_extra_info = get_project_extra_info ctx
    }
  in
  let cmti_iterator =
    { (default_group_iterator ()) with
      actions =
        { (default_iterator_actions ()) with
          begin_of_function_sig = begin_of_cmti_function ctx
        ; end_of_function_sig = end_of_function ctx
        ; begin_of_module_sig = begin_of_cmti_module ctx
        ; end_of_module_sig = (fun _ -> exit_from_module ctx ())
        }
    }
  in
  cmt_iterator, cmti_iterator
;;
