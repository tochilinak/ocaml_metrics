  $ dune build
  $ mylinter . | sed -re "/cyclomatic_complexity|FUNCTION|lines_of_code/p" -n
  FUNCTION fac3 <19:0-19:27> in Factorial.ml
  cyclomatic_complexity_rec: 1.00
  cyclomatic_complexity: 1.00
  lines_of_code: 1.00
  FUNCTION fac3helper <17:0-17:78> in Factorial.ml
  cyclomatic_complexity_rec: 3.00
  cyclomatic_complexity: 2.00
  lines_of_code: 1.00
  FUNCTION fac2 <3:0-14:3> in Factorial.ml
  cyclomatic_complexity_rec: 3.00
  cyclomatic_complexity: 3.00
  lines_of_code: 12.00
  FUNCTION fac1 <1:0-1:55> in Factorial.ml
  cyclomatic_complexity_rec: 3.00
  cyclomatic_complexity: 2.00
  lines_of_code: 1.00
