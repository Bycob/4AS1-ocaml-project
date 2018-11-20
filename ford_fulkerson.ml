open Graph

let find_path 

;;

let rec create_residual_graph (gr : 'a graph) : match gr with
  | [] -> []
	(* Pour tout node ... *)
  | (id_node, arcs) :: rest_gr -> (id_node,
		begin match arcs with
			| [] -> []
			(* ... on crée un arc inverse de poids *)
			| (next_node, poids) :: rest_arcs ->
				(* ... dont le poids prend en compte celui de l'arc inverse (s'il existe) ... *)
				( if find_arc gr next_node id_node = Some (_, poids_inverse)
				then add_arc gr id_node next_node (poids_inverse - poids)
				(* ... ou vaut 0 sinon. *)
				else add_arc gr id_node next_node 0 )
									(* Puis on renvoie l'arc avec son poids et on continue le parcours. *)
									:: (next_node, poids) :: rest_arcs
											end ) :: create_residual_graph rest_gr f


;;

let iter_fulkerson

;;

let ford_fulkerson 

;;