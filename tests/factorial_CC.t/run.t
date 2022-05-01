  $ dune build
  $ mylinter . -met-list CC | sed -re "/^$/d"
  FILE Factorial.ml
  MODULE Factorial in Factorial.ml
  ____Function_metrics____
  FUNCTION fac1 <1:0-1:55> in Factorial.ml
  CC-based_CC-ord: 2
  CC-based_CC-rec: 3
  CC-based_CC-mod: 2
  FUNCTION fac2 <3:0-14:3> in Factorial.ml
  CC-based_CC-ord: 3
  CC-based_CC-rec: 3
  CC-based_CC-mod: 3
  FUNCTION fac3helper <17:0-17:78> in Factorial.ml
  CC-based_CC-ord: 2
  CC-based_CC-rec: 3
  CC-based_CC-mod: 2
  FUNCTION fac3 <19:0-19:27> in Factorial.ml
  CC-based_CC-ord: 1
  CC-based_CC-rec: 1
  CC-based_CC-mod: 1
