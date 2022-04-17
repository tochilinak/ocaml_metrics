let f = ()

let id x = x

let g = fun x -> id x

let sq list =
  match list with
  | [] -> [0] (* <=> 0 :: [] *)
  | x :: xs -> (x * x) :: xs
;;

let labelled ~f (* <=> ~f:f *) ~label:g x = g (f x)

let part f x = labelled ~f (* <=> ~f:f *) x

type record = {
    mutable field : int
}

let inc_field x = x.field <- x.field + 1

let inc (a, b, c) arr {field=y} =
    let tup = (a + 1, b + 1, c + 1) in
    let record = {field = y + 1} in
    match arr with
    | [| x |] -> (tup, [| x + 1 |], record)
    | _ -> (tup, [||], record)
;;
