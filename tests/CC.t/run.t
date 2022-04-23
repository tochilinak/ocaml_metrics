  $ dune build
  $ mylinter . -met-list CC | sed -re "/^$/d"
  FILE Functions.ml
  ____Function_metrics____
  FUNCTION f <1:0-1:50> in Functions.ml
      CC-based_CC: 3
  CC-based_CC-rec: 3
  FUNCTION f1 <3:0-3:53> in Functions.ml
      CC-based_CC: 3
  CC-based_CC-rec: 3
  FUNCTION g <5:0-5:27> in Functions.ml
      CC-based_CC: 2
  CC-based_CC-rec: 2
