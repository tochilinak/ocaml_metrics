  $ dune build
  $ mylinter . -v-list Halstead -met-list Halstead | sed -re "/^$/d"
  Analyzing file: _build/default/Functions.ml
  FILE Functions.ml
  Declared functions:
  f <1:0-1:10>
  id <3:0-3:12>
  g <5:0-5:21>
  sq <7:0-10:28>
  labelled <13:0-13:51>
  part <15:0-15:43>
  inc_field <21:0-21:40>
  inc <23:0-28:30>
  h <31:0-31:16>
  h1 <33:0-33:37>
  ____Function_metrics____
  FUNCTION f <1:0-1:10> in Functions.ml
  Halstead_n: 2
  Halstead_N: 2
  Halstead_V: 2.00
  Halstead_D: 0.00
  Halstead_E: 0.00
  _______extra_info_______
  operators:
  operands:
  < construct () > used 1 times
  < id f > used 1 times
  FUNCTION id <3:0-3:12> in Functions.ml
  Halstead_n: 2
  Halstead_N: 3
  Halstead_V: 3.00
  Halstead_D: 0.00
  Halstead_E: 0.00
  _______extra_info_______
  operators:
  operands:
  < id id > used 1 times
  < id x > used 2 times
  FUNCTION g <5:0-5:21> in Functions.ml
  Halstead_n: 3
  Halstead_N: 4
  Halstead_V: 6.34
  Halstead_D: 0.75
  Halstead_E: 4.75
  _______extra_info_______
  operators:
  < id id > used 1 times
  operands:
  < id g > used 1 times
  < id x > used 2 times
  FUNCTION sq <7:0-10:28> in Functions.ml
  Halstead_n: 9
  Halstead_N: 16
  Halstead_V: 50.72
  Halstead_D: 2.75
  Halstead_E: 139.48
  _______extra_info_______
  operators:
  < construct :: > used 3 times
  < id Stdlib.* > used 1 times
  < Texp_match > used 1 times
  operands:
  < id xs > used 2 times
  < id x > used 3 times
  < id list > used 2 times
  < const 0 > used 1 times
  < id sq > used 1 times
  < construct [] > used 2 times
  FUNCTION labelled <13:0-13:51> in Functions.ml
  Halstead_n: 6
  Halstead_N: 7
  Halstead_V: 18.09
  Halstead_D: 1.25
  Halstead_E: 22.62
  _______extra_info_______
  operators:
  < id g > used 1 times
  < id f > used 1 times
  operands:
  < id x > used 2 times
  < id labelled > used 1 times
  < id g > used 1 times
  < id f > used 1 times
  FUNCTION part <15:0-15:43> in Functions.ml
  Halstead_n: 4
  Halstead_N: 6
  Halstead_V: 12.00
  Halstead_D: 0.83
  Halstead_E: 10.00
  _______extra_info_______
  operators:
  < id labelled > used 1 times
  operands:
  < id part > used 1 times
  < id x > used 2 times
  < id f > used 2 times
  FUNCTION inc_field <21:0-21:40> in Functions.ml
  Halstead_n: 6
  Halstead_N: 9
  Halstead_V: 23.26
  Halstead_D: 1.75
  Halstead_E: 40.71
  _______extra_info_______
  operators:
  < Texp_setfield > used 1 times
  < id Stdlib.+ > used 1 times
  operands:
  < const 1 > used 1 times
  < id x > used 3 times
  < id inc_field > used 1 times
  < field field > used 2 times
  FUNCTION inc <23:0-28:30> in Functions.ml
  Halstead_n: 18
  Halstead_N: 44
  Halstead_V: 183.48
  Halstead_D: 6.75
  Halstead_E: 1238.47
  _______extra_info_______
  operators:
  < Texp_let > used 2 times
  < array > used 3 times
  < record > used 2 times
  < Texp_match > used 1 times
  < tuple > used 4 times
  < id Stdlib.+ > used 5 times
  operands:
  < id y > used 2 times
  < id arr > used 2 times
  < id record > used 3 times
  < const 1 > used 5 times
  < id x > used 2 times
  < id c > used 2 times
  < field field > used 2 times
  < _ > used 1 times
  < id b > used 2 times
  < id a > used 2 times
  < id tup > used 3 times
  < id inc > used 1 times
  FUNCTION h <31:0-31:16> in Functions.ml
  Halstead_n: 3
  Halstead_N: 4
  Halstead_V: 6.34
  Halstead_D: 0.75
  Halstead_E: 4.75
  _______extra_info_______
  operators:
  < id g > used 1 times
  operands:
  < id x > used 2 times
  < id h > used 1 times
  FUNCTION h1 <33:0-33:37> in Functions.ml
  Halstead_n: 9
  Halstead_N: 10
  Halstead_V: 31.70
  Halstead_D: 2.40
  Halstead_E: 76.08
  _______extra_info_______
  operators:
  < array > used 1 times
  < id inc > used 1 times
  < id id > used 1 times
  < tuple > used 1 times
  operands:
  < const 1 > used 1 times
  < id x > used 2 times
  < id h1 > used 1 times
  < const 2 > used 1 times
  < const 3 > used 1 times
