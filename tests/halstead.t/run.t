  $ dune build
  $ mylinter . -v-list Halstead -met-list Halstead | sed -re "/^$/d"
  Analyzing file: _build/default/Functions.ml
  FILE Functions.ml
  ______File_metrics______
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
  Halstead_effort: 0.00
  Halstead_difficulty: 0.00
  Halstead_volume: 2.00
  Halstead_length: 2
  Halstead_vocabulary: 2
  _______extra_info_______
  operators:
  operands:
  < construct () > used 1 times
  < id f > used 1 times
  FUNCTION id <3:0-3:12> in Functions.ml
  Halstead_effort: 0.00
  Halstead_difficulty: 0.00
  Halstead_volume: 3.00
  Halstead_length: 3
  Halstead_vocabulary: 2
  _______extra_info_______
  operators:
  operands:
  < id id > used 1 times
  < id x > used 2 times
  FUNCTION g <5:0-5:21> in Functions.ml
  Halstead_effort: 4.75
  Halstead_difficulty: 0.75
  Halstead_volume: 6.34
  Halstead_length: 4
  Halstead_vocabulary: 3
  _______extra_info_______
  operators:
  < id id > used 1 times
  operands:
  < id g > used 1 times
  < id x > used 2 times
  FUNCTION sq <7:0-10:28> in Functions.ml
  Halstead_effort: 139.48
  Halstead_difficulty: 2.75
  Halstead_volume: 50.72
  Halstead_length: 16
  Halstead_vocabulary: 9
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
  Halstead_effort: 22.62
  Halstead_difficulty: 1.25
  Halstead_volume: 18.09
  Halstead_length: 7
  Halstead_vocabulary: 6
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
  Halstead_effort: 10.00
  Halstead_difficulty: 0.83
  Halstead_volume: 12.00
  Halstead_length: 6
  Halstead_vocabulary: 4
  _______extra_info_______
  operators:
  < id labelled > used 1 times
  operands:
  < id part > used 1 times
  < id x > used 2 times
  < id f > used 2 times
  FUNCTION inc_field <21:0-21:40> in Functions.ml
  Halstead_effort: 40.71
  Halstead_difficulty: 1.75
  Halstead_volume: 23.26
  Halstead_length: 9
  Halstead_vocabulary: 6
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
  Halstead_effort: 1238.47
  Halstead_difficulty: 6.75
  Halstead_volume: 183.48
  Halstead_length: 44
  Halstead_vocabulary: 18
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
  Halstead_effort: 4.75
  Halstead_difficulty: 0.75
  Halstead_volume: 6.34
  Halstead_length: 4
  Halstead_vocabulary: 3
  _______extra_info_______
  operators:
  < id g > used 1 times
  operands:
  < id x > used 2 times
  < id h > used 1 times
  FUNCTION h1 <33:0-33:37> in Functions.ml
  Halstead_effort: 76.08
  Halstead_difficulty: 2.40
  Halstead_volume: 31.70
  Halstead_length: 10
  Halstead_vocabulary: 9
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
