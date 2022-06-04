open Base

let edges_set_1 = [ 0, 1; 1, 2; 3, 4 ]

let () =
  let graph = Graph.init_graph 5 in
  List.iter edges_set_1 ~f:(fun (x, y) -> Graph.add_edge graph x y);
  assert (Int.equal (Algo.count_comp graph) 2)
;;

(*
  AC_1 = 1
  AC_2 = 1
  AC_3 = 1 - 1/2 = 0.5
*)
