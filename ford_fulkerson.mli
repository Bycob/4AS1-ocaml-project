open Graph

type gpath = (id * 'a) list

val ford_fulkerson: 'a graph -> ('a graph * 'a)
