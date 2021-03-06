open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils
open Zanuda_core.METRIC

type context =
  { mutable code_lines : int
  ; mutable all_lines : int
  ; mutable last_structure_item : bool
  }

let default_ctx () = { code_lines = 0; all_lines = 0; last_structure_item = false }
let metrics_group_id = "LOC-based"
let before_module ctx _ = ctx.last_structure_item <- false

let before_function ctx _ =
  ctx.code_lines <- 0;
  ctx.all_lines <- 0
;;

let get_function_metrics_result ctx () =
  [ "code", Int_result ctx.code_lines; "all", Int_result ctx.all_lines ]
;;

let get_lines loc =
  let open Location in
  let _, start_line, _ = get_pos_info loc.loc_start in
  let _, end_line, _ = get_pos_info loc.loc_end in
  start_line, end_line
;;

let count_lines file_content loc =
  let start_line, end_line = get_lines loc in
  let line_nums = range start_line end_line in
  Sequence.counti line_nums ~f:(fun _ x -> not @@ String.equal file_content.(x) "")
;;

let remove_comment_lines file_content (from, till) =
  let iter_line line init_bal =
    (* last coordinate is true if comment_line *)
    String.fold
      file_content.(line)
      ~init:(' ', init_bal, true, true)
      ~f:(fun (c1, bal1, r1, r2) c2 ->
        let bal = max bal1 0 in
        match c1, c2 with
        | '(', '*' -> c2, bal + 1, r2, r1
        | '*', ')' -> c2, bal - 1, r2, r2
        | _ -> if bal = 0 && c2 != ' ' then c2, bal, r2, false else c2, bal, r2, r2)
  in
  let rec go balance line =
    if line <= till
    then (
      file_content.(line) <- String.strip file_content.(line);
      let _, new_bal, _, is_comment = iter_line line balance in
      if is_comment then file_content.(line) <- "";
      go new_bal (line + 1))
  in
  go 0 from
;;

let remove_const file_content loc =
  let open Location in
  let _, l1, p1 = get_pos_info loc.loc_start in
  let _, l2, p2 = get_pos_info loc.loc_end in
  let line_nums = range l1 l2 in
  Sequence.iter line_nums ~f:(fun x ->
      file_content.(x)
        <- String.mapi file_content.(x) ~f:(fun i c ->
               match x with
               | y when y = l1 && l1 = l2 -> if i >= p1 && i < p2 then '#' else c
               | y when y = l1 -> if i >= p1 then '#' else c
               | y when y = l2 -> if i < p2 then '#' else c
               | _ -> '#'))
;;

let expr_pat =
  let open Tast_pattern in
  econst drop
;;

let pat_pat =
  let open Tast_pattern in
  pconst drop
;;

let parse processed_file_content value loc pat =
  Tast_pattern.parse
    pat
    loc
    ~on_error:(fun _desc () -> ())
    value
    (fun () -> remove_const processed_file_content loc)
    ()
;;

let process_pattern : type k. k Tast_pattern.gen_pat -> string array -> unit =
 fun pat processed_file_content ->
  match Tast_pattern.convert_gen_pat pat with
  | Value x -> parse processed_file_content x pat.pat_loc pat_pat
  | _ -> ()
;;

let run ctx _ file_content fallback =
  let processed_file_content = Array.copy file_content in
  let open Tast_iterator in
  { fallback with
    structure_item =
      (fun self str_item ->
        ctx.last_structure_item <- true;
        fallback.structure_item self str_item;
        ctx.last_structure_item <- false)
  ; value_binding =
      (fun self vb ->
        let loc = vb.vb_loc in
        let is_child_of_str_item = ctx.last_structure_item in
        ctx.last_structure_item <- false;
        fallback.value_binding self vb;
        ctx.last_structure_item <- is_child_of_str_item;
        if is_child_of_str_item && not loc.loc_ghost
        then (
          let s, e = get_lines loc in
          remove_comment_lines processed_file_content (s, e);
          ctx.all_lines <- e - s + 1;
          ctx.code_lines <- count_lines processed_file_content loc
          (*print_endline "";
            Array.iteri processed_file_content ~f:(fun x s ->
                let from, till = get_lines loc in
                if x >= from && x <= till then print_endline s)*)))
  ; expr =
      (fun self expr ->
        if not expr.exp_loc.Location.loc_ghost
        then parse processed_file_content expr expr.exp_loc expr_pat;
        fallback.expr self expr)
  ; pat =
      (fun self pat ->
        if not pat.pat_loc.Location.loc_ghost
        then process_pattern pat processed_file_content;
        fallback.pat self pat)
  }
;;

let get_iterator_builder () =
  let ctx = default_ctx () in
  let cmt_iterator =
    { actions =
        { (default_iterator_actions ([], [])) with
          begin_of_function = before_function ctx
        ; begin_of_module = before_module ctx
        ; end_of_function = (fun _ -> get_function_metrics_result ctx (), [])
        }
    ; run = run ctx
    }
  in
  { default_metrics_group_iterator_builder with cmt = cmt_iterator }
;;
