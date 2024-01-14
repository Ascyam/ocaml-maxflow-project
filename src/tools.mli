open Graph

type flow =
  {
    acu: int;
    capa: int;
  }

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: flow graph -> id -> id -> flow -> flow graph

type flow_f =
  {
    acuf: float;
    capaf: float;
  }

val add_arc_f: flow_f graph -> id -> id -> flow_f -> flow_f graph
