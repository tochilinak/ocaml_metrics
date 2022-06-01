  $ dune build
  $ ocaml_metrics . -met-list coupling -v-list coupling \
  > -sec-list test_coupling_algorithm,usage | sed -re "s/ *$//" | sed -re "/^$/d"
  Analyzing file: _build/default/Usage.ml
  Analyzing file: _build/default/Graph.ml
  Analyzing file: _build/default/Algo.ml
  Analyzing file: _build/default/Algo.mli
  _______extra_info_______
  Coupling graph:
  digraph G {
    "usage/1.Usage";
    "Graph";
    "Algo";
    "usage/1.Usage" -> "Algo" [label="1", ];
    "usage/1.Usage" -> "Graph" [label="2", ];
    "Algo" -> "Graph" [label="0", ];
    }
  LIBRARY test_coupling_algorithm
  FILE Graph.ml
  Declared modules:
  Graph
  MODULE Graph in Graph.ml
  _____Module_metrics_____
  coupling_Fan-out: 0
   coupling_Fan-in: 2
     coupling_APIU: 1.00
       coupling_AC: 0.50
      coupling_EXT: 0
  Declared functions:
  init_graph <8:0-9:77>
  add_edge <12:0-14:31>
  ____Function_metrics____
  FUNCTION init_graph <8:0-9:77> in Graph.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Base.Array.make_matrix
  Graph.n
  FUNCTION add_edge <12:0-14:31> in Graph.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Graph.g
  Graph.u
  Graph.v
  FILE Algo.ml
  Declared modules:
  Algo
  MODULE Algo in Algo.ml
  _____Module_metrics_____
  coupling_Fan-out: 1
   coupling_Fan-in: 1
     coupling_APIU: 1.00
       coupling_AC: 0.50
      coupling_EXT: 0
  Declared functions:
  range <4:0-7:29>
  count_comp <10:0-27:8>
  ____Function_metrics____
  FUNCTION range <4:0-7:29> in Algo.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Algo.from
  Algo.till
  Algo.x
  Base.+
  Base.>
  Base.Sequence.unfold
  FUNCTION count_comp <10:0-27:8> in Algo.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Algo.acc
  Algo.dfs
  Algo.g
  Algo.go
  Algo.n
  Algo.range
  Algo.u
  Algo.v
  Algo.visited
  Base.&&
  Base.+
  Base.-
  Base.>=
  Base.Array.create
  Base.Sequence.iter
  Base.not
  EXECUTABLE usage
  FILE Usage.ml
  Declared modules:
  usage/1.Usage
  MODULE usage/1.Usage in Usage.ml
  _____Module_metrics_____
  coupling_Fan-out: 2
   coupling_Fan-in: 0
     coupling_APIU: 0.00
       coupling_AC: 0.50
      coupling_EXT: 3
  Declared functions:
  edges_set_1 <3:0-3:38>
  <Value on 5:0-8:46>
  ____Function_metrics____
  FUNCTION edges_set_1 <3:0-3:38> in Usage.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  FUNCTION <Value on 5:0-8:46> in Usage.ml
  coupling_EXT: 3
  _______extra_info_______
  Called from function:
  Algo.count_comp
  Base.Int.equal
  Base.List.iter
  Graph.add_edge
  Graph.init_graph
  usage/1.Usage.edges_set_1
  usage/1.Usage.graph
  usage/1.Usage.x
  usage/1.Usage.y
  $ ocaml_metrics . -met-list coupling -v-list coupling \
  > -sec-list strange_cases | sed -re "s/ *$//" | sed -re "/^$/d"
  Analyzing file: _build/default/strange_cases/non_api_calls.ml
  Analyzing file: _build/default/strange_cases/non_api_calls.mli
  _______extra_info_______
  Coupling graph:
  digraph G {
    "Strange_cases.Non_api_calls.A";
    "Strange_cases.Non_api_calls";
    "Strange_cases.Non_api_calls" -> "Strange_cases.Non_api_calls.A" [label="2",
                                                                      ];
    }
  LIBRARY strange_cases
  FILE strange_cases/non_api_calls.ml
  Declared modules:
  Strange_cases.Non_api_calls
  Strange_cases.Non_api_calls.A
  MODULE Strange_cases.Non_api_calls in strange_cases/non_api_calls.ml
  _____Module_metrics_____
  coupling_Fan-out: 1
   coupling_Fan-in: 0
     coupling_APIU: 0.00
       coupling_AC: 0.00
      coupling_EXT: 2
  Declared functions:
  calc_sq <6:0-6:28>
  calc_cb <7:0-7:27>
  ____Function_metrics____
  FUNCTION calc_sq <6:0-6:28> in strange_cases/non_api_calls.ml
  coupling_EXT: 1
  _______extra_info_______
  Called from function:
  Strange_cases.Non_api_calls.A.private_func
  FUNCTION calc_cb <7:0-7:27> in strange_cases/non_api_calls.ml
  coupling_EXT: 1
  _______extra_info_______
  Called from function:
  Strange_cases.Non_api_calls.A.public_func
  MODULE Strange_cases.Non_api_calls.A in strange_cases/non_api_calls.ml
  _____Module_metrics_____
  coupling_Fan-out: 0
   coupling_Fan-in: 1
     coupling_APIU: 0.00
       coupling_AC: 0.00
      coupling_EXT: 0
  Declared functions:
  private_func <2:2-2:28>
  public_func <3:2-3:31>
  ____Function_metrics____
  FUNCTION private_func <2:2-2:28> in strange_cases/non_api_calls.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Stdlib.*
  Strange_cases.Non_api_calls.A.x
  FUNCTION public_func <3:2-3:31> in strange_cases/non_api_calls.ml
  coupling_EXT: 0
  _______extra_info_______
  Called from function:
  Stdlib.*
  Strange_cases.Non_api_calls.A.x
