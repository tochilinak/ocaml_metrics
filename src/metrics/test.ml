open Base

let%test_unit "test_remove_comment_lines_1" =
  let file_content = [| ""; "   (* some comment *)  " |] in
  [%test_eq: string array] (LOC.remove_comment_lines file_content) [| ""; "" |]
;;

let%test_unit "test_remove_comment_lines_2" =
  let file_content =
    [| ""
     ; "a(* begin of comment  "
     ; " continiue (*comment*) "
     ; " end of comment *) "
     ; " not a comment "
    |]
  in
  [%test_eq: string array]
    (LOC.remove_comment_lines file_content)
    [| ""; "a(* begin of comment"; ""; ""; "not a comment" |]
;;

let%test_unit "test_remove_comment_lines_3" =
  let file_content =
    [| ""
     ; "  (* begin of comment  "
     ; " continiue comment "
     ; " end of comment *)a "
     ; " not a comment "
    |]
  in
  [%test_eq: string array]
    (LOC.remove_comment_lines file_content)
    [| ""; ""; ""; "end of comment *)a"; "not a comment" |]
;;

let%test_unit "test_remove_comment_lines_4" =
  let file_content = [| ""; "  (* comment 1 *) (* comment 2 *)  " |] in
  [%test_eq: string array] (LOC.remove_comment_lines file_content) [| ""; "" |]
;;
