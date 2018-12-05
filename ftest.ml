open Graph
open Ford_fulkerson

(** Tests *)

let test_graph_map infile outfile =
  (* Open file *)
  let graph = Gfile.from_file infile in

  let graph_int = Graph.map graph int_of_string in

  let graph_int2 = Graph.map graph_int (fun i -> 2 * i) in

  let graph_reversed = Graph.map graph_int2 string_of_int in

  (* Rewrite the graph that has been read. *)
  let () = Gfile.export outfile graph_reversed in

  ()

let unit_test_ford_fulkerson infile outfile =
  let graph = Gfile.from_file infile in
  let int_graph = Graph.map graph int_of_string in (

    let path = Ford_fulkerson.find_path int_graph "0" "5" [] in
    Ford_fulkerson.display_path path
    ;
    let ff_graph = Ford_fulkerson.create_residual_graph int_graph in
    let writable_graph = Graph.map ff_graph string_of_int in
    Gfile.export outfile writable_graph
  )
;;

let test_ford_fulkerson infile source sink outfile =
  let graph = Gfile.from_file infile in
  let int_graph = Graph.map graph int_of_string in

  let (ff_graph, _) = Ford_fulkerson.ford_fulkerson int_graph "0" "5" in

  let writable_graph = Graph.map ff_graph string_of_int in
  let () = Gfile.export outfile writable_graph in
  ()

(** Main *)

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and _source = Sys.argv.(2)
  and _sink = Sys.argv.(3)
  in

  let () = test_ford_fulkerson infile _source _sink outfile in
  ()
