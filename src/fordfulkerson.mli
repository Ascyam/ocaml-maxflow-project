open Graph

type capacite = int
type flow = int 

type fcarc = (flow * capacite)

type path = (id * id * fcarc) list

type graphf = fcarc graph

val init_flow : 'a graph -> fcarc graph 

val diff_graph : fcarc graph -> fcarc graph

(*val graphinter : int graph -> int graph*) 

val find_path : flow graph -> id -> id -> path

val print_path : path -> unit