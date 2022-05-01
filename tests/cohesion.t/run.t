  $ dune build
  $ mylinter . -met-list cohesion | sed -re "/^$/d"
  FILE Cohesion.ml
  MODULE Cohesion.A in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 0.30
   cohesion_LCOM1: 6
   cohesion_LCOM2: 2
  cohesion_LCOM34: 2
   cohesion_LCOM5: 1.40
