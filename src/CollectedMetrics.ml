open Base
open Caml.Format
open Utils

type context =
  { file_list : string Queue.t
  ; functions_in_file : (string, string list) Hashtbl.t
  ; metric_results : (string, (string * metric_result) list) Hashtbl.t
  ; metric_extra_info : (string, string list) Hashtbl.t
  ; mutable longest_file_metrics : int
  ; mutable longest_func_metrics : int
  }

let ctx : context =
  { file_list = Queue.create ()
  ; functions_in_file = Hashtbl.create (module String)
  ; metric_results = Hashtbl.create (module String)
  ; metric_extra_info = Hashtbl.create (module String)
  ; longest_file_metrics = 0
  ; longest_func_metrics = 0
  }
;;

let add_value table key value =
  Hashtbl.update table key ~f:(fun v ->
      match v with
      | None -> [ value ]
      | Some list -> value :: list)
;;

let add_file = Queue.enqueue ctx.file_list
let add_function filename func = add_value ctx.functions_in_file filename func

let add_file_result filename metric_id res =
  ctx.longest_file_metrics <- max ctx.longest_file_metrics (String.length metric_id);
  add_value ctx.metric_results filename (metric_id, res)
;;

let key_for_func filename func_name = filename ^ ":" ^ func_name

let add_func_result filename func_name metric_id res =
  ctx.longest_func_metrics <- max ctx.longest_func_metrics (String.length metric_id);
  add_value ctx.metric_results (key_for_func filename func_name) (metric_id, res)
;;

let add_extra_info where extra_info =
  Hashtbl.update ctx.metric_extra_info where ~f:(fun v ->
      match v with
      | None -> extra_info
      | Some list -> list @ ("" :: extra_info))
;;

let add_extra_info_file filename = add_extra_info filename

let add_extra_info_func filename func_name =
  add_extra_info (key_for_func filename func_name)
;;

let print_extra_info verbose where =
  if verbose
  then (
    match Hashtbl.find ctx.metric_extra_info where with
    | None | Some [] -> ()
    | Some x ->
      Format.printf "\n_______extra_info_______\n\n";
      List.iter x ~f:(fun s -> Format.printf "%s\n" s);
      Format.printf "\n")
;;

let print_metric width metric_id value =
  let fstr = "%" ^ Int.to_string width ^ "s: " in
  Format.printf (Scanf.format_from_string fstr "%s") metric_id;
  match value with
  | Int_result x -> Format.printf "%d\n" x
  | Float_result x -> Format.printf "%.2f\n" x
;;

let print_func_metrics verbose filename func =
  let key = filename ^ ":" ^ func in
  let metrics = List.rev @@ Hashtbl.find_exn ctx.metric_results key in
  Format.printf "FUNCTION %s in %s\n" func filename;
  List.iter metrics ~f:(fun (x, y) -> print_metric ctx.longest_func_metrics x y);
  print_extra_info verbose key;
  Format.printf "\n"
;;

let default_find dict key =
  match Hashtbl.find dict key with
  | None -> []
  | Some list -> list
;;

let print_file_metrics verbose filename =
  let metrics = List.rev @@ default_find ctx.metric_results filename in
  let functions = List.rev @@ default_find ctx.functions_in_file filename in
  Format.printf "FILE %s\n" filename;
  Format.printf "\n______File_metrics______\n\n";
  List.iter metrics ~f:(fun (x, y) -> print_metric ctx.longest_file_metrics x y);
  print_extra_info verbose filename;
  Format.printf "\nDeclared functions:\n";
  List.iter functions ~f:(fun x -> Format.printf "%s\n" x);
  Format.printf "\n____Function_metrics____\n\n";
  List.iter functions ~f:(print_func_metrics verbose filename);
  Format.printf "\n"
;;

let report verbose () = Queue.iter ctx.file_list ~f:(print_file_metrics verbose)
