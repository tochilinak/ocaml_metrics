let rec f x =
  match x with
  (* empty list *)
  | [] -> 0
  (* not empty list *)
  | _::xs -> 1 + f xs
;;

(*
  some big comment
  .
  .
  .
*) (* comment *) let g () = ();; let g1 () = 0

let h str =
  print_endline "h";

  match str with
  | "(*" -> 0
  | _ -> 1
;;

let outer () =
  let module A = struct

      let inner x =

          x * x
      ;;

      end
  in
  (*  *)
  let _ = A.inner 1 in
  ()
;;
