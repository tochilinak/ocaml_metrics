open Base
open Caml.Format
open Utils

let metric_notes : string Queue.t = Queue.create ()
let add_note = Queue.enqueue metric_notes

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
let print_metric metric_id value = Format.printf "%s: %.2f\n" metric_id value

let print_func_metrics filename func =
  let key = filename ^ ":" ^ func in
  let metrics = Hashtbl.find_exn metric_results key in
  Format.printf "FUNCTION %s\n" func;
  List.iter metrics ~f:(fun (x, y) -> print_metric x y);
  Format.printf "\n"
;;

let print_file_metrics filename =
  let metrics = Hashtbl.find_exn metric_results filename in
  let functions =
    match Hashtbl.find functions_in_file filename with
    | None -> []
    | Some list -> list
  in
  Format.printf "FILE %s\n" filename;
  Format.printf "\n______File_metrics______\n\n";
  List.iter metrics ~f:(fun (x, y) -> print_metric x y);
  Format.printf "\nDeclared functions:\n";
  List.iter functions ~f:(fun x -> Format.printf "%s\n" x);
  Format.printf "\n____Function_metrics____\n\n";
  List.iter functions ~f:(print_func_metrics filename);
  Format.printf "\n"
;;

let report verbose () =
  Queue.iter file_list ~f:print_file_metrics;
  let report_notes () =
    Format.printf "\n";
    Queue.iter metric_notes ~f:(fun s -> Format.printf "%s\n" s)
  in
  if verbose () then report_notes ()
;;
