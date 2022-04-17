open Base
open Caml.Format
open Utils

let add_value table key value =
  Hashtbl.update table key ~f:(fun v ->
      match v with
      | None -> [ value ]
      | Some list -> value :: list)
;;

let file_list : string Queue.t = Queue.create ()
let add_file = Queue.enqueue file_list
let functions_in_file : (string, string list) Hashtbl.t = Hashtbl.create (module String)
let add_function filename func = add_value functions_in_file filename func

let metric_results : (string, (string * float) list) Hashtbl.t =
  Hashtbl.create (module String)
;;

let add_result metric_id where res = add_value metric_results where (metric_id, res)
let metric_extra_info : (string, string list) Hashtbl.t = Hashtbl.create (module String)

let add_extra_info where extra_info =
  Hashtbl.update metric_extra_info where ~f:(fun v ->
      match v with
      | None -> extra_info
      | Some list -> list @ ("" :: extra_info))
;;

let print_extra_info verbose where =
  if verbose
  then (
    match Hashtbl.find metric_extra_info where with
    | None | Some [] -> ()
    | Some x ->
      Format.printf "\n_______extra_info_______\n\n";
      List.iter x ~f:(fun s -> Format.printf "%s\n" s);
      Format.printf "\n")
;;

let print_metric metric_id value = Format.printf "%s: %.2f\n" metric_id value

let print_func_metrics verbose filename func =
  let key = filename ^ ":" ^ func in
  let metrics = Hashtbl.find_exn metric_results key in
  Format.printf "FUNCTION %s in %s\n" func filename;
  List.iter metrics ~f:(fun (x, y) -> print_metric x y);
  print_extra_info verbose key;
  Format.printf "\n"
;;

let default_find dict key =
  match Hashtbl.find dict key with
  | None -> []
  | Some list -> list
;;

let print_file_metrics verbose filename =
  let metrics = default_find metric_results filename in
  let functions = default_find functions_in_file filename in
  Format.printf "FILE %s\n" filename;
  Format.printf "\n______File_metrics______\n\n";
  List.iter metrics ~f:(fun (x, y) -> print_metric x y);
  print_extra_info verbose filename;
  Format.printf "\nDeclared functions:\n";
  List.iter functions ~f:(fun x -> Format.printf "%s\n" x);
  Format.printf "\n____Function_metrics____\n\n";
  List.iter functions ~f:(print_func_metrics verbose filename);
  Format.printf "\n"
;;

let report verbose () = Queue.iter file_list ~f:(print_file_metrics verbose)
