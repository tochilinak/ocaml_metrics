  $ dune build
  $ ocaml_metrics . -met-list cohesion -v-list cohesion | sed -re "s/ *$//" | sed -re "/^$/d"
  Analyzing file: _build/default/Cohesion.ml
  LIBRARY test_cohesion
  FILE Cohesion.ml
  Declared modules:
  Cohesion.A
  Cohesion.B
  Cohesion.C
  Cohesion.D
  Cohesion.D.E
  Cohesion.F
  Cohesion.G
  Cohesion.G.<module at 55:21-58:9>
  MODULE Cohesion.A in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 0.30
   cohesion_LCOM1: 6
   cohesion_LCOM2: 2
  cohesion_LCOM34: 2
   cohesion_LCOM5: 1.40
  Declared functions:
  f1 <2:2-2:14>
  f2 <3:2-3:13>
  f3 <4:2-4:13>
  f4 <5:2-5:18>
  f5 <6:2-6:13>
  _______extra_info_______
  Maximum possible arcs: 10
  COHESION GRAPH:
  digraph G {
    "f4 <5:2-5:18>";
    "f1 <2:2-2:14>";
    "f3 <4:2-4:13>";
    "f2 <3:2-3:13>";
    "f5 <6:2-6:13>";
    "f3 <4:2-4:13>" -> "f1 <2:2-2:14>";
    "f2 <3:2-3:13>" -> "f1 <2:2-2:14>";
    "f5 <6:2-6:13>" -> "f4 <5:2-5:18>";
    }
  MODULE Cohesion.B in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 0.50
   cohesion_LCOM1: 0
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: 1.00
  Declared functions:
  f <16:2-16:57>
  g <17:2-17:59>
  _______extra_info_______
  Maximum possible arcs: 4
  COHESION GRAPH:
  digraph G {
    "f <16:2-16:57>";
    "g <17:2-17:59>";
    "f <16:2-16:57>" -> "g <17:2-17:59>";
    "g <17:2-17:59>" -> "f <16:2-16:57>";
    }
  MODULE Cohesion.C in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 1.00
   cohesion_LCOM1: 1
   cohesion_LCOM2: 1
  cohesion_LCOM34: 2
   cohesion_LCOM5: -1.00
  Declared functions:
  f <24:2-24:17>
  g <25:2-25:21>
  _______extra_info_______
  Maximum possible arcs: 0
  COHESION GRAPH:
  digraph G {
    "g <25:2-25:21>";
    "f <24:2-24:17>";
    }
  MODULE Cohesion.D in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 1.00
   cohesion_LCOM1: 0
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: -1.00
  Declared functions:
  mod_name <30:2-30:20>
  print_mod_name <31:2-31:48>
  _______extra_info_______
  Maximum possible arcs: 1
  COHESION GRAPH:
  digraph G {
    "mod_name <30:2-30:20>";
    "print_mod_name <31:2-31:48>";
    "print_mod_name <31:2-31:48>" -> "mod_name <30:2-30:20>";
    }
  MODULE Cohesion.D.E in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 1.00
   cohesion_LCOM1: 0
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: -1.00
  Declared functions:
  mod_name_E <34:4-34:26>
  print_mod_name <35:4-35:52>
  _______extra_info_______
  Maximum possible arcs: 1
  COHESION GRAPH:
  digraph G {
    "print_mod_name <35:4-35:52>";
    "mod_name_E <34:4-34:26>";
    "print_mod_name <35:4-35:52>" -> "mod_name_E <34:4-34:26>";
    }
  MODULE Cohesion.F in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 0.67
   cohesion_LCOM1: 1
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: 1.00
  Declared functions:
  a,b <41:2-41:21>
  print_a <42:2-42:34>
  print_b <43:2-43:34>
  <Value on 45:2-47:16>
  _______extra_info_______
  Maximum possible arcs: 6
  COHESION GRAPH:
  digraph G {
    "print_a <42:2-42:34>";
    "print_b <43:2-43:34>";
    "a,b <41:2-41:21>";
    "<Value on 45:2-47:16>";
    "print_a <42:2-42:34>" -> "a,b <41:2-41:21>";
    "print_b <43:2-43:34>" -> "a,b <41:2-41:21>";
    "<Value on 45:2-47:16>" -> "print_a <42:2-42:34>";
    "<Value on 45:2-47:16>" -> "print_b <43:2-43:34>";
    }
  MODULE Cohesion.G in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 0.67
   cohesion_LCOM1: 1
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: -1.00
  Declared functions:
  f1 <52:2-52:14>
  outer <54:2-61:11>
  f2 <64:2-64:22>
  _______extra_info_______
  Maximum possible arcs: 3
  COHESION GRAPH:
  digraph G {
    "f1 <52:2-52:14>";
    "outer <54:2-61:11>";
    "f2 <64:2-64:22>";
    "outer <54:2-61:11>" -> "f1 <52:2-52:14>";
    "f2 <64:2-64:22>" -> "outer <54:2-61:11>";
    }
  MODULE Cohesion.G.<module at 55:21-58:9> in Cohesion.ml
  _____Module_metrics_____
     cohesion_COH: 1.00
   cohesion_LCOM1: 0
   cohesion_LCOM2: 0
  cohesion_LCOM34: 1
   cohesion_LCOM5: -1.00
  Declared functions:
  inner <56:10-56:27>
  _______extra_info_______
  Maximum possible arcs: 0
  COHESION GRAPH:
  digraph G {
    "inner <56:10-56:27>";
    }
