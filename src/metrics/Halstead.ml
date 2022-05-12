open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC

type context =
  { operand_dictionary : (string, int) Hashtbl.t
  ; operator_dictionary : (string, int) Hashtbl.t
  ; mutable last_apply : bool
  }

let default_ctx () =
  { operand_dictionary = Hashtbl.create (module String)
  ; operator_dictionary = Hashtbl.create (module String)
  ; last_apply = false
  }
;;

let metrics_group_id = "Halstead"

let before_function ctx _ =
  Hashtbl.clear ctx.operand_dictionary;
  Hashtbl.clear ctx.operator_dictionary;
  ctx.last_apply <- false
;;

let add_to_dict dict operator_name =
  Hashtbl.update dict operator_name ~f:(fun v ->
      match v with
      | None -> 1
      | Some x -> x + 1)
;;

let apply_operator = [ "id Stdlib.@@"; "id Base.@@" ]
let not_operator = "Texp_function" :: apply_operator

let change_name : (string, string) Hashtbl.t =
  Hashtbl.of_alist_exn
    ~growth_allowed:false
    (module String)
    [ "Texp_tuple", "tuple"
    ; "Tpat_tuple", "tuple"
    ; "Texp_array", "array"
    ; "Tpat_array", "array"
    ; "Texp_record", "record"
    ; "Tpat_record", "record"
    ]
;;

let add_operator ctx x =
  if List.mem not_operator x ~equal:String.equal
  then ()
  else (
    let name =
      match Hashtbl.find change_name x with
      | Some y -> y
      | None -> x
    in
    add_to_dict ctx.operator_dictionary name)
;;

let add_operand ctx = add_to_dict ctx.operand_dictionary

let calc_total_sum dict =
  Hashtbl.fold dict ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
;;

let calc_total_operators ctx () = calc_total_sum ctx.operator_dictionary
let calc_total_operands ctx () = calc_total_sum ctx.operand_dictionary
let calc_dist_operators ctx () = Hashtbl.length ctx.operator_dictionary
let calc_dist_operands ctx () = Hashtbl.length ctx.operand_dictionary

let get_function_metrics_result ctx () =
  let dist_operators = calc_dist_operators ctx () in
  let dist_operands = calc_dist_operands ctx () in
  let total_operators = calc_total_operators ctx () in
  let total_operands = calc_total_operands ctx () in
  let f = float_of_int in
  let dist_sum = f dist_operators +. f dist_operands in
  let total_sum = f total_operators +. f total_operands in
  let vol = Float.log dist_sum /. Float.log 2. *. total_sum in
  let diff =
    if dist_operators != 0
    then f dist_operators /. 2. *. (f total_operands /. f dist_operands)
    else 0.
  in
  let eff = vol *. diff in
  [ "n", Int_result (dist_operators + dist_operands)
  ; "N", Int_result (total_operators + total_operands)
  ; "V", Float_result vol
  ; "D", Float_result diff
  ; "E", Float_result eff
  ]
;;

let get_function_extra_info ctx () =
  let get_str_list dict =
    Hashtbl.fold dict ~init:[] ~f:(fun ~key ~data acc ->
        Format.sprintf "< %s > used %d times" key data :: acc)
  in
  [ "operators:" ]
  @ get_str_list ctx.operator_dictionary
  @ [ "\noperands:" ]
  @ get_str_list ctx.operand_dictionary
;;

let atom_pat_expr =
  let open Tast_pattern in
  let open Types in
  texp_ident @@ map1 __ ~f:(fun x -> "id " ^ Path.name x)
  ||| map1 (econst __) ~f:(fun x -> "const " ^ const_to_string x)
  ||| texp_construct_empty (map1 __ ~f:(fun x -> "construct " ^ x.cstr_name))
  ||| texp_field drop @@ label_desc (map1 __ ~f:(fun x -> "field " ^ x))
;;

let process_not_atom_expr ctx expr =
  let open Typedtree in
  let get_name = function
    | Texp_construct (_, x, _) -> "construct " ^ x.cstr_name
    | _ -> Names.texp_name expr
  in
  add_operator ctx @@ get_name expr
;;

let process_not_atom_operand_expr ctx expr =
  let open Typedtree in
  let open Asttypes in
  match expr.exp_desc with
  | Texp_setfield (_, _, x, _) -> add_operand ctx @@ "field " ^ x.lbl_name
  | Texp_record { fields } ->
    Array.iter fields ~f:(fun (x, _) -> add_operand ctx @@ "field " ^ x.lbl_name)
  (*| Texp_function { arg_label } ->
    (match arg_label with
    | Labelled s | Optional s -> add_operand @@ "label " ^ s
    | _ -> ())
  | Texp_apply (_, list) ->
    List.iter list ~f:(fun x ->
        match x with
        | _, None -> ()
        | (Labelled s, Some x | Optional s, Some x) when not x.exp_loc.Location.loc_ghost
          -> add_operand @@ "label " ^ s
        | _ -> ())*)
  | _ -> ()
;;

let process_expression ctx expr =
  let open Typedtree in
  let loc = expr.exp_loc in
  Tast_pattern.parse
    atom_pat_expr
    loc
    ~on_error:(fun _desc () ->
      process_not_atom_operand_expr ctx expr;
      match expr.exp_desc with
      | Texp_apply _ -> ctx.last_apply <- true
      | x ->
        ctx.last_apply <- false;
        process_not_atom_expr ctx x)
    expr
    (fun id () ->
      if ctx.last_apply then add_operator ctx id else add_operand ctx id;
      ctx.last_apply <- List.mem apply_operator id ~equal:String.equal)
    ()
;;

let atom_pat_value =
  let open Tast_pattern in
  let open Types in
  tpat_var @@ map1 __ ~f:(fun x -> "id " ^ x)
  ||| map0 tpat_any ~f:"_"
  ||| tpat_construct_empty (map1 __ ~f:(fun x -> "construct " ^ x.cstr_name))
  ||| map1 (pconst __) ~f:(fun x -> "const " ^ const_to_string x)
  ||| tpat_alias @@ map1 __ ~f:(fun x -> "id " ^ x)
;;

let process_not_atom_value ctx pat =
  let open Typedtree in
  let get_name = function
    | Tpat_construct (_, x, _) -> "construct " ^ x.cstr_name
    | _ -> Names.tpat_name pat
  in
  add_operator ctx @@ get_name pat
;;

let process_not_atom_operand_value ctx pat =
  let open Typedtree in
  match pat.pat_desc with
  | Tpat_record (fields, _) ->
    List.iter fields ~f:(fun (_, x, _) -> add_operand ctx @@ "field " ^ x.lbl_name)
  | _ -> ()
;;

let atom_pat_comp =
  let open Tast_pattern in
  map0 (tpat_exception drop) ~f:"exception"
;;

let process_pattern : type k. context -> k Tast_pattern.gen_pat -> unit =
 fun ctx pat ->
  let open Typedtree in
  match Tast_pattern.convert_gen_pat pat with
  | Value x ->
    Tast_pattern.parse
      atom_pat_value
      x.pat_loc
      ~on_error:(fun _desc () ->
        process_not_atom_operand_value ctx x;
        process_not_atom_value ctx x.pat_desc)
      x
      (fun id () -> add_operand ctx id)
      ()
  | Computation x ->
    Tast_pattern.parse
      atom_pat_comp
      x.pat_loc
      ~on_error:(fun _desc () -> ())
      x
      (fun id () -> add_operator ctx id)
      ()
  | Or_pattern -> add_operator ctx "Tpat_or"
;;

let format_construct expr =
  let open Location in
  let open Typedtree in
  match expr.exp_desc with
  | Texp_construct (_, desc, _) ->
    (match get_pos_info desc.cstr_loc.loc_start with
    | "camlinternalFormatBasics.mli", _, _ -> true
    | _ -> false)
  | _ -> false
;;

let run ctx _ _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        if not (format_construct expr) then process_expression ctx expr;
        fallback.expr self expr)
  ; pat =
      (fun self pat ->
        process_pattern ctx pat;
        fallback.pat self pat)
  }
;;

let get_iterators () =
  let ctx = default_ctx () in
  let cmt_iterator =
    { actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = before_function ctx
        ; end_of_function =
            (fun _ -> get_function_metrics_result ctx (), get_function_extra_info ctx ())
        }
    ; run = run ctx
    ; collect_delayed_metrics = (fun () -> ())
    ; get_project_extra_info = (fun () -> [])
    }
  in
  cmt_iterator, default_group_iterator ()
;;
