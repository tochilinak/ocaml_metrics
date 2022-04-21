  $ dune build
  $ mylinter . | sed -re "/cyclomatic_complexity|FUNCTION|lines_of_code/p" -n
  FUNCTION fac1 <1:0-1:55> in Factorial.ml
  lines_of_code: 1
  cyclomatic_complexity: 2
  cyclomatic_complexity_rec: 3
  FUNCTION fac2 <3:0-14:3> in Factorial.ml
  lines_of_code: 12
  cyclomatic_complexity: 3
  cyclomatic_complexity_rec: 3
  FUNCTION fac3helper <17:0-17:78> in Factorial.ml
  lines_of_code: 1
  cyclomatic_complexity: 2
  cyclomatic_complexity_rec: 3
  FUNCTION fac3 <19:0-19:27> in Factorial.ml
  lines_of_code: 1
  cyclomatic_complexity: 1
  cyclomatic_complexity_rec: 1
