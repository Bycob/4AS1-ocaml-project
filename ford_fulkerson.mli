open Graph

type gpath = (id * int) list

val ford_fulkerson: int graph -> (int graph * int)
