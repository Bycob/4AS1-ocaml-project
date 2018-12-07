open Graph

type gpath = (id * id * int) list

val display_path: gpath -> unit

val find_path: int graph -> id -> id -> gpath-> gpath

val create_residual_graph: int graph -> int graph

(* Applique l'algorithme de Ford Fulkerson et retourne le graphe résiduel
avec le flot trouvé. *)
val ford_fulkerson: int graph -> id -> id -> (int graph * int)

(* A partir d'un graphe de capacité de flot et d'un graphe résiduel, construit
un graphe de flot. Chaque tag d'arc est de la forme : flot/flot_max *)
val fancy_ff_graph: int graph -> int graph -> string graph
