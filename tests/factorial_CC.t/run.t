  $ dune build
  $ mylinter . -met-list LOC,CC-based | sed -re "/^$/d"
  FILE Factorial.ml
  ______File_metrics______
  Declared functions:
  fac1 <1:0-1:55>
  fac2 <3:0-14:3>
  fac3helper <17:0-17:78>
  fac3 <19:0-19:27>
  ____Function_metrics____
  FUNCTION fac1 <1:0-1:55> in Factorial.ml
    LOC-based_LOC: 1
      CC-based_CC: 2
  CC-based_CC-rec: 3
  FUNCTION fac2 <3:0-14:3> in Factorial.ml
    LOC-based_LOC: 12
      CC-based_CC: 3
  CC-based_CC-rec: 3
  FUNCTION fac3helper <17:0-17:78> in Factorial.ml
    LOC-based_LOC: 1
      CC-based_CC: 2
  CC-based_CC-rec: 3
  FUNCTION fac3 <19:0-19:27> in Factorial.ml
    LOC-based_LOC: 1
      CC-based_CC: 1
  CC-based_CC-rec: 1
