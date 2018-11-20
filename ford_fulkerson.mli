open Graph

type gpath = (id * int) list


val find_path: int graph -> id -> id -> (id * int) list-> gpath

val ford_fulkerson: int graph -> (int graph * int)
