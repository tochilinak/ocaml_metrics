  $ dune build
  $ mylinter . -met-list cohesion | sed -re "/^$/d"
  FILE Cohesion_1.ml
  MODULE Cohesion_1 in Cohesion_1.ml
  _____Module_metrics_____
     cohesion_COH: 0.30
   cohesion_LCOM1: 6
   cohesion_LCOM2: 2
  cohesion_LCOM34: 2
   cohesion_LCOM5: 1.40
