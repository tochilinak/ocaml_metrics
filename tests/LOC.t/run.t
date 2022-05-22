  $ dune build
  $ ocaml_metrics . -met-list LOC | sed -re "/^$/d"
  LIBRARY test_LOC
  FILE Functions.ml
  MODULE Test_LOC.Functions in Functions.ml
  ____Function_metrics____
  FUNCTION f <1:0-6:21> in Functions.ml
  LOC-based_code: 4
   LOC-based_all: 6
  FUNCTION g <14:17-14:30> in Functions.ml
  LOC-based_code: 1
   LOC-based_all: 1
  FUNCTION g1 <14:33-14:46> in Functions.ml
  LOC-based_code: 1
   LOC-based_all: 1
  FUNCTION h <16:0-21:10> in Functions.ml
  LOC-based_code: 5
   LOC-based_all: 6
