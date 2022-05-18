open Base
open Graph

let range from till =
  Sequence.unfold ~init:from ~f:(function
      | x when x > till -> None
      | x -> Some (x, x + 1))
;;

let count_comp g =
  let n = g.vertices_num in
  let visited = Array.create ~len:n false in
  let rec dfs v =
    visited.(v) <- true;
    Sequence.iter (range 0 (n - 1)) ~f:(fun u ->
        if (not visited.(u)) && g.edge_matrix.(v).(u) then dfs u)
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

(*
  AC_1 = 1 - 1/2 = 0.5
  AC_2 = 1 - 1/4 = 0.75
  AC_3 = 1
*)
