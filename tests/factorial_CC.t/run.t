  $ dune build
  $ ocaml_metrics . -met-list CC-based_ord | sed -re "/^$/d"
  LIBRARY test_factorial_CC
  FILE Factorial.ml
  MODULE Factorial in Factorial.ml
  ____Function_metrics____
  FUNCTION fac1 <1:0-1:55> in Factorial.ml
  CC-based_ord: 2
  FUNCTION fac2 <3:0-14:3> in Factorial.ml
  CC-based_ord: 3
  FUNCTION fac3helper <17:0-17:78> in Factorial.ml
  CC-based_ord: 2
  FUNCTION fac3 <19:0-19:27> in Factorial.ml
  CC-based_ord: 1
