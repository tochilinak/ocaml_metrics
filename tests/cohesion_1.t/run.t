  $ dune build
  $ mylinter . -met-list cohesion | sed -re "/^$/d"
  FILE Cohesion_1.ml
  ______File_metrics______
     cohesion_COH: 0.30
   cohesion_LCOM1: 6
   cohesion_LCOM2: 2
  cohesion_LCOM34: 2
   cohesion_LCOM5: 1.40
