open Graph

type gpath = (id * int) list

let rec display_path = function
  | [] -> Printf.printf "\n"
  | (id, tag) :: tail -> Printf.printf "%s : %d -> " id tag; display_path tail
;;

let rec find_path gr id_src id_dest acu =
  if id_src = id_dest then acu
  else
    (* Vérifie que le poids de l'arc n'est pas 0 et que le noeud n'a pas déjà été exploré *)
    let is_valid_arc (id, tag) = tag <> 0 && not (List.exists (fun (other_id, other_tag) -> id = other_id) acu) in
    let next_nodes = List.filter is_valid_arc (out_arcs gr id_src) in
    let finish_path (next_id, a) = find_path gr next_id id_dest ((next_id, a) :: acu) in
    let rec return_path = function
      | [] -> raise Not_found
      | head :: tail -> try finish_path head with Not_found -> return_path tail
    in
    return_path next_nodes
;;

let rec create_residual_graph (gr : 'a graph) = match gr with
  | [] -> []
	(* Pour tout node ... *)
  | (id_node, arcs) :: rest_gr -> (id_node,
		begin match arcs with
			| [] -> []
			(* ... on crée un arc inverse de poids *)
			| (next_node, poids) :: rest_arcs ->
				(* ... dont le poids prend en compte celui de l'arc inverse (s'il existe) ... *)
				( match find_arc gr next_node id_node with
          | Some (_, poids_inverse) -> add_arc gr id_node next_node (poids_inverse - poids)
				(* ... ou vaut 0 sinon. *)
				  | _ -> add_arc gr id_node next_node 0 )
									(* Puis on renvoie l'arc avec son poids et on continue le parcours. *)
									:: (next_node, poids) :: rest_arcs
											end ) :: create_residual_graph rest_gr f

;;

let iter_fulkerson = ();;

let ford_fulkerson (gr : int graph) = gr, 0 ;;
