open Graph

type gpath = (id * id * int) list

let rec display_path = function
  | [] -> Printf.printf "\n"
  | (id_from, id_to, tag) :: tail -> Printf.printf "%s <-- %d -- %s, " id_to tag id_from; display_path tail
;;

let rec find_path gr id_src id_dest acu =
  if id_src = id_dest then acu
  else
    (* Vérifie que le poids de l'arc n'est pas 0 et que le noeud n'a pas déjà été exploré *)
    let is_valid_arc (id, tag) = tag <> 0 && not (List.exists (fun (_, other_id, other_tag) -> id = other_id) acu) in
    (* Récupère la liste des arcs suivants valides *)
    let next_nodes = List.filter is_valid_arc (out_arcs gr id_src) in
    (* fonction essayant de terminer le chemin par appel récursif à "find_path" *)
    let finish_path (next_id, tag) = find_path gr next_id id_dest ((id_src, next_id, tag) :: acu) in
    (* fonction retournant le premier chemin trouvé, à partir d'un des nodes de la liste *)
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

(* Effectue une iteration du ford_fulkerson. Lève l'exception
Not_found si le noeud n'a pas été trouvé. *)
(*TODO lever une exception si id_src = id_dest *)
let iter_fulkerson (gr_resid, prev_flow) id_src id_dest =
  (* cherche un chemin entre id_src et id_dest *)
  let found_path = find_path gr_resid id_src id_dest [] in
  (* cherche le flux maximum qu'on peut rajouter à ce chemin  *)
  let fold_min acu (_, _, tag) = if acu > (abs tag) then (abs tag) else acu in
  let max_flow = List.fold_left fold_min max_int found_path in
  (* applique un noeud dans un sens ou dans l'autre (* le sens est soit 1 soit -1 *) *)
  let apply_arc flow gr id_from id_to = add_arc gr id_from id_to
    (match find_arc gr id_from id_to with
    | None -> raise Not_found (* never raised (normally) *)
    | Some tag -> tag - flow)
  in
  let apply_node flow gr (id_from, id_to, tag) =
    let signed_flow = if tag > 0 then flow else -flow in
    apply_arc (-signed_flow) (apply_arc signed_flow gr id_from id_to) id_to id_from
  in
  (List.fold_left (apply_node max_flow) gr_resid found_path, prev_flow + max_flow)
;;

let ford_fulkerson (gr : int graph) id_src id_dst =
  let gr_init = create_residual_graph gr in
  let rec algo_ff (gr_resid, flow) =
    try (
      let (gr_resid_new, flow_new) = iter_fulkerson (gr_resid, flow) id_src id_dst in
      algo_ff (gr_resid_new, flow_new)
    ) with Not_found -> (gr_resid, flow)
  in
  algo_ff (gr_init, 0);;
