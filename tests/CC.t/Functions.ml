let f x = if x >= 2 && x * x < 16 then 10 else -10

let f1 x = if x >= 100 || x * x < 16 then 10 else -10

let g x = try x with _ -> 0
