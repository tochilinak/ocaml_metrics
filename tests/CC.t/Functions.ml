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
