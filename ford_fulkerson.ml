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

let only_nodes gr = v_fold gr (fun accu id _ -> add_node accu id) empty_graph ;;

let create_residual_graph (gr : int graph) =
  (* Fusionne les double arretes sur un graphe, c'est-a-dire les couples d'arretes
   * (id1 -> id2), (id2 -> id1) *)
  let merge_edges gr = v_fold gr (fun accu id_src out_arcs ->
    List.fold_left (fun accu2 (id_dst, lbl) -> match find_arc accu2 id_dst id_src with
      | None -> add_arc accu2 id_src id_dst lbl
      | Some currentLbl -> if (currentLbl > lbl)
        then add_arc accu2 id_dst id_src (currentLbl - lbl)
        else add_arc (remove_arc accu2 id_dst id_src) id_src id_dst (lbl - currentLbl)
      ) accu out_arcs)
    (only_nodes gr)
  in
	v_fold (merge_edges gr) (fun graph_accu id_src out_arcs -> (List.fold_left (fun graph_bis (id_dst, lbl) ->
		let graph_ter = (add_arc graph_bis id_src id_dst lbl) in
		add_arc graph_ter id_dst id_src 0
	) graph_accu out_arcs)) (only_nodes gr)
;;

(* Effectue une iteration du ford_fulkerson. Lève l'exception
 * Not_found aucun chemin n'a été trouvé *)
(*TODO lever une exception si id_src = id_dest *)
let iter_fulkerson (gr_resid, prev_flow) id_src id_dest =
  (* cherche un chemin entre id_src et id_dest *)
  let found_path = find_path gr_resid id_src id_dest [] in
  (* cherche le flux maximum qu'on peut rajouter à ce chemin  *)
  let fold_min acu (_, _, tag) = if acu > (abs tag) then (abs tag) else acu in
  let max_flow = List.fold_left fold_min max_int found_path in
  (* applique un noeud du path dans un sens ou dans l'autre (* le sens est soit 1 soit -1 *) *)
  let apply_arc flow gr id_from id_to = add_arc gr id_from id_to
    (match find_arc gr id_from id_to with
    | None -> raise Not_found (* never raised (normally) *)
    | Some tag -> tag - flow)
  in
  (* applique un noeud du path dans les deux sens, en prenant en compte le type d'arrête *)
  let apply_node flow gr (id_from, id_to, tag) =
    let signed_flow = if tag > 0 then flow else -flow in
    apply_arc signed_flow (apply_arc signed_flow gr id_from id_to) id_to id_from
  in
  (List.fold_left (apply_node max_flow) gr_resid found_path, prev_flow + max_flow)
;;

(* Construit un graphe résultat de l'algorithme de fulkerson à partir d'un graphe
initial et d'un graphe résiduel. *)
let fancy_ff_graph gr gr_resid =
  (* fonction retournant une string de la forme flow_count/flow_max pour une arrête du graphe initial. *)
  let get_tag id_src id_dst flow_max = match find_arc gr_resid id_dst id_src with
    | None -> raise Not_found (* never raised if gr_resid is valid *)
    | Some flow_count -> (string_of_int (- flow_count)) ^ "/" ^ (string_of_int flow_max)
  in
  (* ajoute un arc d'un noeud du graphe initial au graphe résultat *)
  let change_single_arc id_src acu (id_dst, tag) = add_arc acu id_src id_dst (get_tag id_src id_dst tag) in
  (* ajoute tous les arcs d'un noeud du graphe initial au graphe résultat *)
  let change_arcs_for_node acu id_src out_arcs = List.fold_left (change_single_arc id_src) acu out_arcs in
  v_fold gr change_arcs_for_node (only_nodes gr);;

let ford_fulkerson (gr : int graph) id_src id_dst =
  let gr_init = create_residual_graph gr in
  let rec algo_ff (gr_resid, flow) iter =
    try (
      let (gr_resid_new, flow_new) = iter_fulkerson (gr_resid, flow) id_src id_dst in
      let iter_new = if iter > 100 then () else Gfile.export ("debug/debug" ^ (string_of_int iter) ^ ".dot") (fancy_ff_graph gr gr_resid); iter + 1 in
      algo_ff (gr_resid_new, flow_new) iter_new
    ) with Not_found -> (gr_resid, flow)
  in
  algo_ff (gr_init, 0) 0;;
