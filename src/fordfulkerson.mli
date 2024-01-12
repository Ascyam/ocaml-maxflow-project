open Graph

type flow =
  {
    acu: int;
    capa: int
  }

val init_graphf : int graph -> flow graph 

(*val diff_graph : (flow*flow) graph -> flow graph*)

(*val path_flow : path option -> flow

val path_capacite : path option -> capa*)

val find_path : flow graph -> id -> id -> flow arc list

val print_path : flow arc list -> unit