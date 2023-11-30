open Graph

type capacite = int
type flow = int 

type fcarc = flow*capacite

type path = id list

val init_flow : 'a graph -> fcarc graph 

(*val find_path : int graph -> id list -> id -> id -> path option *)