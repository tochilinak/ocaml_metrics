open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type graph =
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

let count_comp g =
  let n = g.vertices_num in
  let visited = Array.create ~len:n false in
  let rec dfs v =
    visited.(v) <- true;
    Sequence.iter
      (range 0 (n - 1))
      ~f:(fun u -> if (not visited.(u)) && g.edge_matrix.(v).(u) then dfs u)
  in
  let rec go v acc =
    if v >= n
    then acc
    else if visited.(v)
    then go (v + 1) acc
    else (
      dfs v;
      (go [@tailcall]) (v + 1) (acc + 1))
  in
  go 0 0
;;
