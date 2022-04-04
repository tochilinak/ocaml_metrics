open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

let metric_id = "lines_of_code"
let extra_info () = []
let result = ref 0
let last_structure_item = ref false
let reset () = last_structure_item := false
let inner_reset () = result := 0
let get_result () = [ "", float_of_int !result ]

let count_lines file_content loc =
  let open Location in
  let _, start_line, _ = get_pos_info loc.loc_start in
  let _, end_line, _ = get_pos_info loc.loc_end in
  let line_nums =
    Sequence.unfold ~init:start_line ~f:(function
        | x when x > end_line -> None
        | x -> Some (x, x + 1))
  in
  Sequence.counti line_nums ~f:(fun _ x -> not @@ String.equal file_content.(x) "")
;;

let remove_comment_lines file_content =
  let result = Array.copy file_content in
  let iter_line line init_bal =
    (* last coordinate is true if comment_line *)
    String.fold
      result.(line)
      ~init:(' ', init_bal, true, true)
      ~f:(fun (c1, bal, r1, r2) c2 ->
        match c1, c2 with
        | '(', '*' -> c2, bal + 1, r2, r1
        | '*', ')' -> c2, bal - 1, r2, r2
        | _ -> if bal = 0 && c2 != ' ' then c2, bal, r2, false else c2, bal, r2, r2)
  in
  let rec go balance line =
    if line < Array.length result
    then (
      result.(line) <- String.strip result.(line);
      let _, new_bal, _, is_comment = iter_line line balance in
      if is_comment then result.(line) <- "";
      go new_bal (line + 1))
  in
  go 0 0;
  result
;;

let run _ file_content fallback =
  let processed_file_content = remove_comment_lines file_content in
  (*Array.iter processed_file_content ~f:print_endline;*)
  let open Tast_iterator in
  { fallback with
    structure_item =
      (fun self str_item ->
        last_structure_item := true;
        fallback.structure_item self str_item;
        last_structure_item := false)
  ; value_binding =
      (fun self vb ->
        let is_child_of_str_item = !last_structure_item in
        if is_child_of_str_item
        then result := count_lines processed_file_content vb.vb_loc;
        last_structure_item := false;
        fallback.value_binding self vb;
        last_structure_item := is_child_of_str_item)
  }
;;
