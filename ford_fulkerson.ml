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

let create_residual_graph (gr : int graph) =
	let only_nodes = v_fold gr (fun graph_accu id _ -> add_node graph_accu id) empty_graph in
	v_fold gr (fun graph_accu id_src out_arcs -> (List.fold_left (fun graph_bis (id_dst, lbl) ->
		let graph_ter = (add_arc graph_bis id_src id_dst lbl) in
		add_arc graph_ter id_dst id_src 0
	) graph_accu out_arcs)) only_nodes
;;

let iter_fulkerson = ();;

let ford_fulkerson (gr : int graph) = gr, 0 ;;
