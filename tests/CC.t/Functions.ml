let f x = if x >= 2 && x * x < 16 then 10 else -10

let f1 x = if x >= 100 || x * x < 16 then 10 else -10

let g x = try x with _ -> 0

let h str =
  match str with
  | "Hello" -> 0
  | "Goodbuy" -> 1
  | "..." -> 2
  | _ -> 3
;;

let rec k x = if x <= 0 then 0 else 1 + k (x - 1)

let m list =
  match list with
  | [] -> ""
  | [_] -> "1"
  | x::_ when x < 0 -> "-"
  | _ -> "+"
;;

let rec pat list =
  match list with
  | [] | [_] -> "0/1"
  | _::xs -> "1::" ^ pat xs
;;

let rec_func x =
  let rec helper x acc =
      let _inner_func () = () in
      if x <= 0 then acc else helper (x - 1) (acc * x)
  and _g x = if x <= 0 then 0 else _g (x - 1) in
  helper x 1
;;

let p x =
  let p = 1 in
  x + p
;;

let outer () =
  if true then () else ();
  let module A = struct
      let inner () = if true then () else ()
      end
  in
  if true then () else ();
  A.inner
;;
