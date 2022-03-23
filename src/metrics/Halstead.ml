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

let not_operator = [ "ident @@"; "Texp_function" ]

let add_operator x =
  if List.mem not_operator x ~equal:String.equal
  then ()
  else add_to_dict operator_dictionary x
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
  Hashtbl.iteri operator_dictionary ~f:(fun ~key ~data ->
      Format.printf "operator %s %d\n" key data);
  Hashtbl.iteri operand_dictionary ~f:(fun ~key ~data ->
      Format.printf "operand %s %d\n" key data);
  let _n1 = float_of_int (calc_dist_operators ()) in
  let _n2 = float_of_int (calc_dist_operands ()) in
  let _N1 = float_of_int (calc_total_operators ()) in
  let _N2 = float_of_int (calc_total_operands ()) in
  let _n = _n1 +. _n2 in
  let _N = _N1 +. _N2 in
  let vol = Float.log _n /. Float.log 2. *. _N in
  let diff = _n1 /. 2. *. (_N2 /. _n2) in
  let eff = vol *. diff in
  [ ":volume", vol; ":difficulty", diff; ":effort", eff ]
;;

let atom_pat_expr =
  let open Tast_pattern in
  let open Types in
  texp_ident @@ map1 apply ~f:(fun x -> "ident " ^ Path.last x)
  ||| map1 (econst apply) ~f:(fun x -> "const " ^ const_to_string x)
  ||| texp_construct_visible_empty (map1 apply ~f:(fun x -> "construct " ^ x.cstr_name))
;;

(*let loc_printer loc =
  Location.print_loc Format.str_formatter loc;
  String.drop_prefix (String.drop_suffix (Format.flush_str_formatter ()) 4) 4
;;*)

let process_not_atom expr =
  let open Typedtree in
  let get_name = function
    | Texp_construct (_, x, _) -> "Texp_construct " ^ x.cstr_name
    | _ -> Texp_names.texp_name expr
  in
  add_operator @@ get_name expr
;;

(*print_endline @@ "operator: " ^ (get_name expr)*)

let run _ fallback =
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          atom_pat_expr
          loc
          ~on_error:(fun _desc () ->
            match expr.exp_desc with
            | Texp_apply _ -> last_apply := true
            | x ->
              last_apply := false;
              process_not_atom x)
          expr
          (fun id () ->
            if !last_apply then add_operator id else add_operand id;
            last_apply := false)
          ();
        (*print_endline (loc_printer expr.exp_loc);*)
        fallback.expr self expr)
  }
;;
