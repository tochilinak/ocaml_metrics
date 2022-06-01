module A = struct
  let a1 () = ()
  let a2 () = ()

  module E = struct
    let e1 = a1
  end
end

module B = struct
  let b1 = A.a1
  let b2 = A.a1
end

module C = struct
  let c1 lst f = List.iter f lst
  let c2 = B.b2
  let c3 = A.a2
end
