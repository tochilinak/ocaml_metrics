  $ dune build
  $ ocaml_metrics . -met-list CC | sed -re "/^$/d"
  LIBRARY test_CC
  FILE Functions.ml
  MODULE Test_CC.Functions in Functions.ml
  _____Module_metrics_____
  CC-based_mod-ord-max: 5
  CC-based_mod-ord-avg: 3.17
  ____Function_metrics____
  FUNCTION f <1:0-1:50> in Functions.ml
  CC-based_ord: 3
  CC-based_rec: 3
  CC-based_mod: 3
  FUNCTION f1 <3:0-3:53> in Functions.ml
  CC-based_ord: 3
  CC-based_rec: 3
  CC-based_mod: 3
  FUNCTION g <5:0-5:27> in Functions.ml
  CC-based_ord: 2
  CC-based_rec: 2
  CC-based_mod: 2
  FUNCTION h <7:0-12:10> in Functions.ml
  CC-based_ord: 4
  CC-based_rec: 4
  CC-based_mod: 2
  FUNCTION k <15:0-15:49> in Functions.ml
  CC-based_ord: 2
  CC-based_rec: 3
  CC-based_mod: 2
  FUNCTION m <17:0-22:12> in Functions.ml
  CC-based_ord: 5
  CC-based_rec: 5
  CC-based_mod: 3
