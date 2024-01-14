open Graph
open Tools

val init_graphf : id graph -> flow graph 

val find_path : flow graph -> id -> id -> flow arc list

val print_path : flow arc list -> unit

val diff_graph : flow graph -> flow graph

val string_of_flow : flow -> string

val update_flow : flow graph -> flow arc list -> id -> flow graph

val flow_min : flow arc list -> id

val ford_fulkerson : id graph -> id -> id -> flow graph

(*Float*)
val init_graphf_f : float graph -> flow_f graph 

val find_path_f : flow_f graph -> id -> id -> flow_f arc list

val string_of_flow_f : flow_f -> string

val update_flow_f : flow_f graph -> flow_f arc list -> float -> flow_f graph

val flow_min_f : flow_f arc list -> float

val ford_fulkerson_f : float graph -> id -> id -> flow_f graph