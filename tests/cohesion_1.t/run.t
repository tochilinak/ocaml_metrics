  $ dune build
  $ mylinter . -met-list cohesion | sed -re "/^$/d"
  FILE Cohesion_1.ml
  ______File_metrics______
     cohesion_COH: 0.30
   cohesion_LCOM1: 6
   cohesion_LCOM2: 2
  cohesion_LCOM34: 2
   cohesion_LCOM5: 1.40
  ____Function_metrics____
  FUNCTION f1 <1:0-1:12> in Cohesion_1.ml
  FUNCTION f2 <3:0-3:11> in Cohesion_1.ml
  FUNCTION f3 <5:0-5:11> in Cohesion_1.ml
  FUNCTION f4 <7:0-7:16> in Cohesion_1.ml
  FUNCTION f5 <9:0-9:11> in Cohesion_1.ml
