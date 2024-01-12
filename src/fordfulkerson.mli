open Graph
open Tools

val init_graphf : int graph -> flow graph 

val diff_graph : flow graph -> flow graph

val find_path : flow graph -> id -> id -> flow arc list

val print_path : flow arc list -> unit

val update_flow : flow graph -> flow arc list -> int -> flow graph

val flow_min : flow arc list -> int

val ford_fulkerson : int graph -> id -> id -> flow graph

val string_of_flow : flow -> string