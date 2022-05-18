open Base
open Caml.Format
open METRIC

module Item = struct
  type t =
    | Root
    | Executable of string * int (* name, id *)
    | Library of string * int (* name, id *)
    | File of string (* filename *)
    | Module of string * string (* filename, modname *)
    | Function of string * string * string (* filename, modname, func_name *)

  let to_string = function
    | Root -> ""
    | Executable (x, id) -> x ^ "/" ^ Format.sprintf "%d" id
    | Library (x, id) -> x ^ "/" ^ Format.sprintf "%d" id
    | File x -> x
    | Module (x, y) -> x ^ ":" ^ y
    | Function (x, y, z) -> x ^ ":" ^ y ^ ":" ^ z
  ;;

  let last_id = ref 0

  let construct_executable exe_name =
    last_id := !last_id + 1;
    Executable (exe_name, !last_id)
  ;;

  let construct_library lib_name =
    last_id := !last_id + 1;
    Library (lib_name, !last_id)
  ;;

  let hash x = String.hash (to_string x)
  let compare x y = String.compare (to_string x) (to_string y)
  let sexp_of_t x = String.sexp_of_t (to_string x)
end

type context =
  { declarations : (Item.t, Item.t list) Hashtbl.t
  ; metric_results : (Item.t, (string * metric_result) list) Hashtbl.t
  ; metric_extra_info : (Item.t, string list) Hashtbl.t
  }

let ctx : context =
  { declarations = Hashtbl.create (module Item)
  ; metric_results = Hashtbl.create (module Item)
  ; metric_extra_info = Hashtbl.create (module Item)
  }
;;

let add_value ~table ~key ~value =
  Hashtbl.update table key ~f:(fun v ->
      match v with
      | None -> [ value ]
      | Some list -> value :: list)
;;

let add_declaration = add_value ~table:ctx.declarations

let add_executable exe_name =
  let exe = Item.construct_executable exe_name in
  add_declaration ~key:Root ~value:exe;
  exe
;;

let add_library lib_name =
  let lib = Item.construct_library lib_name in
  add_declaration ~key:Root ~value:lib;
  lib
;;

let add_file where filename = add_declaration ~key:where ~value:(File filename)

let add_module filename modname =
  add_declaration ~key:(File filename) ~value:(Module (filename, modname))
;;

let add_function filename modname func =
  add_declaration
    ~key:(Module (filename, modname))
    ~value:(Function (filename, modname, func))
;;

let add_result where metric_id res =
  add_value ~table:ctx.metric_results ~key:where ~value:(metric_id, res)
;;

let f_on_module f filename modname = f @@ Item.Module (filename, modname)

let f_on_func f filename modname func_name =
  f @@ Item.Function (filename, modname, func_name)
;;

let add_module_result = f_on_module add_result
let add_func_result = f_on_func add_result

let add_extra_info where extra_info =
  if not @@ List.is_empty extra_info
  then
    Hashtbl.update ctx.metric_extra_info where ~f:(fun v ->
        match v with
        | None -> extra_info
        | Some list -> list @ ("" :: extra_info))
;;

let add_extra_info_module = f_on_module add_extra_info
let add_extra_info_func = f_on_func add_extra_info
let add_extra_info_project = add_extra_info Item.Root

module Printer = struct
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

  let rec print_metric ?(is_rec = false) width metric_id value =
    let print_name () =
      let fstr = "%" ^ Int.to_string width ^ "s: " in
      Format.printf (Scanf.format_from_string fstr "%s") metric_id
    in
    match value with
    | Int_result x ->
      print_name ();
      Format.printf "%d\n" x
    | Float_result x ->
      print_name ();
      Format.printf "%.2f\n" x
    | Delayed_result x ->
      (match !x with
      | Some y, _ ->
        assert (not is_rec);
        print_metric ~is_rec:true width metric_id y
      | None, true -> ()
      | None, _ ->
        (* metric wasn't calculated *)
        Format.eprintf "Metric %s wasn't calculated\n" metric_id;
        assert false)
  ;;

  let default_find dict key =
    match Hashtbl.find dict key with
    | None -> []
    | Some list -> list
  ;;

  let get_metrics key = List.rev @@ default_find ctx.metric_results key

  let fold_on_results ~init ~f =
    Hashtbl.fold ctx.metric_results ~init ~f:(fun ~key ~data acc ->
        List.fold data ~init:acc ~f:(fun x y -> f x key y))
  ;;

  let longest_func_metrics () =
    fold_on_results ~init:0 ~f:(fun acc key (x, _) ->
        match key with
        | Function _ -> max acc (String.length x)
        | _ -> acc)
  ;;

  let function_metrics_added () = longest_func_metrics () > 0

  let longest_module_metrics () =
    fold_on_results ~init:0 ~f:(fun acc key (x, _) ->
        match key with
        | Module _ -> max acc (String.length x)
        | _ -> acc)
  ;;

  let print_func_metrics verbose filename modname func =
    let key = Item.Function (filename, modname, func) in
    let width = longest_func_metrics () in
    let metrics = get_metrics key in
    Format.printf "FUNCTION %s in %s\n" func filename;
    List.iter metrics ~f:(fun (x, y) -> print_metric width x y);
    print_extra_info verbose key;
    Format.printf "\n"
  ;;

  let item_names items =
    List.map items ~f:(function
        | Item.Root -> ""
        | Item.File x
        | Item.Module (_, x)
        | Item.Function (_, _, x)
        | Item.Executable (x, _)
        | Item.Library (x, _) -> x)
  ;;

  let get_subitems key = item_names @@ List.rev (default_find ctx.declarations key)

  let print_module_metrics verbose filename modname =
    let key = Item.Module (filename, modname) in
    let metrics = get_metrics key in
    let width = longest_module_metrics () in
    let functions = get_subitems key in
    Format.printf "MODULE %s in %s\n" modname filename;
    if not (List.is_empty metrics) then Format.printf "\n_____Module_metrics_____\n\n";
    List.iter metrics ~f:(fun (x, y) -> print_metric width x y);
    if verbose
    then (
      Format.printf "\nDeclared functions:\n";
      List.iter functions ~f:(Format.printf "%s\n"));
    if function_metrics_added ()
    then (
      Format.printf "\n____Function_metrics____\n\n";
      List.iter functions ~f:(print_func_metrics verbose filename modname));
    print_extra_info verbose key;
    Format.printf "\n"
  ;;

  let print_file_info verbose filename =
    let key = Item.File filename in
    let modules =
      item_names
      @@ List.rev
      @@ List.filter (default_find ctx.declarations key) ~f:(Hashtbl.mem ctx.declarations)
    in
    Format.printf "FILE %s\n" filename;
    if verbose
    then (
      Format.printf "\nDeclared modules:\n";
      List.iter modules ~f:(Format.printf "%s\n"));
    Format.printf "\n";
    List.iter modules ~f:(fun x -> print_module_metrics verbose filename x)
  ;;

  let print_section_info verbose section =
    let filenames = get_subitems section in
    (match section with
    | Item.Executable (x, _) -> Format.printf "EXECUTABLE %s\n" x
    | Item.Library (x, _) -> Format.printf "LIBRARY %s\n" x
    | _ -> assert false);
    Format.printf "\n";
    List.iter filenames ~f:(print_file_info verbose)
  ;;

  let report verbose () =
    print_extra_info verbose Item.Root;
    List.iter (default_find ctx.declarations Item.Root) ~f:(print_section_info verbose)
  ;;
end
