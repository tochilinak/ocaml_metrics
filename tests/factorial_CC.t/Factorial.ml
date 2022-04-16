let rec fac1 n = if n <= 1 then 1 else n * fac1 (n - 1)

let fac2 n =
  if n <= 1
  then 1
  else (
    let m = ref n in
    let acc = ref 1 in
    while !m > 1 do
      acc := !acc * !m;
      m := !m - 1
    done;
    !acc
  )
;;

let rec fac3helper n acc = if n < 1 then acc else fac3helper (n - 1) (acc * n)

let fac3 n = fac3helper n 1
