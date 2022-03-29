open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let metric_id = "halstead"
let operand_dictionary : (string, int) Hashtbl.t = Hashtbl.create (module String)
let operator_dictionary : (string, int) Hashtbl.t = Hashtbl.create (module String)
let last_apply = ref false

let reset () =
  Hashtbl.clear operand_dictionary;
  Hashtbl.clear operator_dictionary;
  last_apply := false
;;

let add_to_dict dict operator_name =
  Hashtbl.update dict operator_name ~f:(fun v ->
      match v with
      | None -> 1
      | Some x -> x + 1)
;;

let not_operator = [ "id @@"; "Texp_function" ]

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

let add_operator x =
  if List.mem not_operator x ~equal:String.equal
  then ()
  else (
    let name =
      match Hashtbl.find change_name x with
      | Some y -> y
      | None -> x
    in
    add_to_dict operator_dictionary name)
;;

let add_operand = add_to_dict operand_dictionary

let calc_total_sum dict =
  Hashtbl.fold dict ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)
;;

let calc_total_operators () = calc_total_sum operator_dictionary
let calc_total_operands () = calc_total_sum operand_dictionary
let calc_dist_operators () = Hashtbl.length operator_dictionary
let calc_dist_operands () = Hashtbl.length operand_dictionary

let get_result () =
  let _n1 = float_of_int (calc_dist_operators ()) in
  let _n2 = float_of_int (calc_dist_operands ()) in
  let _N1 = float_of_int (calc_total_operators ()) in
  let _N2 = float_of_int (calc_total_operands ()) in
  let _n = _n1 +. _n2 in
  let _N = _N1 +. _N2 in
  let vol = Float.log _n /. Float.log 2. *. _N in
  let diff = _n1 /. 2. *. (_N2 /. _n2) in
  let eff = vol *. diff in
  [ "_vocabulary", _n1 +. _n2
  ; "_length", _N1 +. _N2
  ; "_volume", vol
  ; "_difficulty", diff
  ; "_effort", eff
  ]
;;

let extra_info () =
  let get_str_list dict name =
    Hashtbl.fold dict ~init:[] ~f:(fun ~key ~data acc ->
        Format.sprintf "%s < %s > used %d times" name key data :: acc)
  in
  get_str_list operator_dictionary "operator" @ get_str_list operand_dictionary "operand"
;;

let atom_pat_expr =
  let open Tast_pattern in
  let open Types in
  texp_ident @@ map1 apply ~f:(fun x -> "id " ^ Path.last x)
  ||| map1 (econst apply) ~f:(fun x -> "const " ^ const_to_string x)
  ||| texp_construct_empty (map1 apply ~f:(fun x -> "construct " ^ x.cstr_name))
  ||| texp_field drop @@ label_desc (map1 apply ~f:(fun x -> "field " ^ x))
;;

let process_not_atom_expr expr =
  let open Typedtree in
  let get_name = function
    | Texp_construct (_, x, _) -> "construct " ^ x.cstr_name
    | _ -> Names.texp_name expr
  in
  add_operator @@ get_name expr
;;

let process_not_atom_operand expr =
  let open Typedtree in
  match expr.exp_desc with
  | Texp_setfield (_, _, x, _) -> add_operand @@ "field " ^ x.lbl_name
  | _ -> ()
;;

let process_expression expr =
  let open Typedtree in
  let loc = expr.exp_loc in
  Tast_pattern.parse
    atom_pat_expr
    loc
    ~on_error:(fun _desc () ->
      process_not_atom_operand expr;
      match expr.exp_desc with
      | Texp_apply _ -> last_apply := true
      | x ->
        last_apply := false;
        process_not_atom_expr x)
    expr
    (fun id () ->
      if !last_apply then add_operator id else add_operand id;
      last_apply := false (*print_endline @@ id ^ " on " ^ (location_str expr.exp_loc)*))
    ()
;;

let atom_pat_value =
  let open Tast_pattern in
  let open Types in
  tpat_var @@ map1 apply ~f:(fun x -> "id " ^ x)
  ||| map0 tpat_any ~f:"_"
  ||| tpat_construct_empty (map1 apply ~f:(fun x -> "construct " ^ x.cstr_name))
  ||| map1 (pconst apply) ~f:(fun x -> "const " ^ const_to_string x)
  ||| tpat_alias @@ map1 apply ~f:(fun x -> "id " ^ x)
;;

let process_not_atom_value pat =
  let open Typedtree in
  let get_name = function
    | Tpat_construct (_, x, _) -> "construct " ^ x.cstr_name
    | _ -> Names.tpat_name pat
  in
  add_operator @@ get_name pat
;;

let atom_pat_comp =
  let open Tast_pattern in
  map0 (tpat_exception drop) ~f:"exception"
;;

let process_pattern : type k. k Typedtree.general_pattern -> unit =
 fun pat ->
  let open Typedtree in
  match Tast_pattern.convert_gen_pat pat with
  | Value x ->
    Tast_pattern.parse
      atom_pat_value
      x.pat_loc
      ~on_error:(fun _desc () -> process_not_atom_value x.pat_desc)
      x
      (fun id () -> add_operand id)
      ()
  | Computation x ->
    Tast_pattern.parse
      atom_pat_comp
      x.pat_loc
      ~on_error:(fun _desc () -> ())
      x
      (fun id () -> add_operator id)
      ()
  | Or_pattern -> add_operator "Tpat_or"
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

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        if not (format_construct expr) then process_expression expr;
        fallback.expr self expr)
  ; pat =
      (fun self pat ->
        process_pattern pat;
        fallback.pat self pat)
  ; case =
      (fun self case ->
        add_operator "case";
        fallback.case self case)
  }
;;
