module A = struct
  let f1 x = x
  let f2 = f1
  let f3 = f1
  let f4 x = x * x
  let f5 = f4
  (* possible arcs:
     f2 -> f1;
     f3 -> f2; f3 -> f1;
     f4 -> f3; f4 -> f2; f4 -> f1;
     f5 -> f4; f5 -> f3; f5 -> f2; f5 -> f1
  *)
end

module B = struct
  let rec f x acc = if x <= 0 then acc else g (x - 1) acc
  and g x acc = if x <= 0 then acc else f (x - 1) (acc + 1)
  (* possible arcs:
     f -> f; f -> g; g -> f; g -> g
  *)
end

module C = struct
  let f x = x * x
  and g x = x * x * x
  (* no possible arcs *)
end

module D = struct
  let mod_name = "D"
  let print_mod_name () = print_endline mod_name

  module E = struct
    let mod_name_E = "D.E"
    let print_mod_name () = print_endline mod_name_E
  end

end

module F = struct
  let a, b = "a", "b"
  let print_a () = print_endline a
  let print_b () = print_endline b

  let () =
      print_a ();
      print_b ()
  ;;
end

module G = struct
  let f1 x = x

  let outer () =
      let module H = struct
          let inner () = ()
          (* no possible arcs *)
      end
      in
      H.inner ();
      f1 ()
  ;;

  let f2 () = outer ()

  (* possible arcs: outer -> f1, f2 -> outer, f2 -> f1 *)
end
