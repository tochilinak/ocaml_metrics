open Base

let%test_unit "test_remove_comment_lines_1" =
  let file_content = [| ""; "   (* some comment *)  " |] in
  LOC.remove_comment_lines file_content (0, 1);
  [%test_eq: string array] file_content [| ""; "" |]
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
  LOC.remove_comment_lines file_content (0, 4);
  [%test_eq: string array]
    file_content
    [| ""; "a(* begin of comment"; ""; ""; "not a comment" |]
;;

let%test_unit "test_remove_comment_lines_3" =
  let file_content =
    [| "  (* begin of comment  "
     ; " continiue comment "
     ; " end of comment *)a "
     ; " not a comment "
    |]
  in
  LOC.remove_comment_lines file_content (0, 3);
  [%test_eq: string array]
    file_content
    [| ""; ""; "end of comment *)a"; "not a comment" |]
;;

let%test_unit "test_remove_comment_lines_4" =
  let file_content = [| ""; "  (* comment 1 *) (* comment 2 *)  " |] in
  LOC.remove_comment_lines file_content (0, 1);
  [%test_eq: string array] file_content [| ""; "" |]
;;

let%test_unit "test_remove_comment_lines_5" =
  let file_content = [| "*) let f = ()"; "(* x *)" |] in
  LOC.remove_comment_lines file_content (0, 1);
  [%test_eq: string array] file_content [| "*) let f = ()"; "" |]
;;

let%test_unit "test_count_comp" =
  let g = Graph.init_graph 5 in
  Graph.add_edge g 0 1;
  Graph.add_edge g 1 2;
  Graph.add_edge g 3 4;
  [%test_eq: int] 2 @@ Graph.count_comp g
;;
