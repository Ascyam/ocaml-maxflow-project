open Graph

type flow =
  {
    acu: int;
    capa: int;
  }

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: flow graph -> id -> id -> flow -> flow graph

