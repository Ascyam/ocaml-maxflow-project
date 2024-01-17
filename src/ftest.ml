open Tools
open Fordfulkerson
open Moneysharing
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;

  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (*We get the personnes, the depenses and we use all the functions we created or modified to print the graph*)
  let personnes = from_file_personnes infile in
  let depenses = from_file_depenses infile in
  let graph_f = flow_remboursement personnes depenses in
  let graph_f = ford_fulkerson_f graph_f _source _sink in
  let graph_f = gmap graph_f (fun x -> string_of_flow_f x) in

  let () = export_money outfile graph_f personnes in

  ()

