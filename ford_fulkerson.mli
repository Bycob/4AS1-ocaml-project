open Graph

type gpath = (id * id * int) list

val display_path: gpath -> unit

val find_path: int graph -> id -> id -> gpath-> gpath

val create_residual_graph: int graph -> int graph

val ford_fulkerson: int graph -> id -> id -> (int graph * int)
