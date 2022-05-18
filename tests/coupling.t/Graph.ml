open Base

type t =
  { vertices_num : int
  ; edge_matrix : bool array array
  }

let init_graph n =
  { vertices_num = n; edge_matrix = Array.make_matrix ~dimx:n ~dimy:n false }
;;

let add_edge g u v =
  g.edge_matrix.(u).(v) <- true;
  g.edge_matrix.(v).(u) <- true
;;

(*
  AC_1 = 1 - 1/2 = 0.5
  AC_2 = 1 - 1/4 = 0.75
  AC_3 = 1
  AC = 0.5
*)
