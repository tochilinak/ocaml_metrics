  $ dune build
  $ ocaml_metrics . -met-list CC | sed -re "/^$/d"
  LIBRARY test_CC
  FILE Functions.ml
  MODULE Functions in Functions.ml
  ____Function_metrics____
  FUNCTION f <1:0-1:50> in Functions.ml
  CC-based_CC-ord: 3
  CC-based_CC-rec: 3
  CC-based_CC-mod: 3
  FUNCTION f1 <3:0-3:53> in Functions.ml
  CC-based_CC-ord: 3
  CC-based_CC-rec: 3
  CC-based_CC-mod: 3
  FUNCTION g <5:0-5:27> in Functions.ml
  CC-based_CC-ord: 2
  CC-based_CC-rec: 2
  CC-based_CC-mod: 2
  FUNCTION h <7:0-12:10> in Functions.ml
  CC-based_CC-ord: 4
  CC-based_CC-rec: 4
  CC-based_CC-mod: 2
  FUNCTION k <15:0-15:49> in Functions.ml
  CC-based_CC-ord: 2
  CC-based_CC-rec: 3
  CC-based_CC-mod: 2
  FUNCTION m <17:0-22:12> in Functions.ml
  CC-based_CC-ord: 5
  CC-based_CC-rec: 5
  CC-based_CC-mod: 3
