open Graph

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
  
  let () = test_graph_map infile outfile in
  ()

