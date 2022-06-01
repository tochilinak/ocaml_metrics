module A = struct
  let private_func x = x * x
  let public_func x = x * x * x
end

let calc_sq = A.private_func
let calc_cb = A.public_func
