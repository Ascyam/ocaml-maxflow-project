open Graph
open Printf

(*I first defined 3 types so that I can stock infos from the file in these types and keep them linked*)
type personne = {
  id : int;
  nom : string;
  solde : float;
}

type depense = {
  personne : int;
  prix : float;
  (*participants : personne list;*) (*This is a way of upgrading this version but it seems to be a bit harder but possible assuming that we change the syntax of the 
     money sharing file and we adapt the from_file_depense function to finally integrate this list in the moyenne_depense and all the others functions using moyenne_depense*)
}

(*The prequel of an arc*)
type dette = {
  creancier : personne;
  debiteur : personne;
  montant : float;
}

(*The two following functions do not need explanation I guess...*)
let new_personne personnes id nom solde = {id=id;nom=nom;solde=solde}::personnes

let new_depense depenses payeur prix = {personne=payeur;prix=prix}::depenses

(*read_personne and read_depense allows me to get the informations from the file by unsing scanf*)
let read_personne personnes line =
  Printf.printf "personne: %s\n%!" line ;
  try Scanf.sscanf line "p %d %s %f" (fun id nom solde -> new_personne personnes id nom solde)
  with e ->
    Printf.printf "Cannot read personne in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let read_depense depenses line =
  Printf.printf "dépense: %s\n%!" line ;
  try Scanf.sscanf line "d %d %f"
        (fun payeur prix -> new_depense depenses payeur prix)
  with e ->
    Printf.printf "Cannot read depense in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

(*This function is used to ignore the comments in the file*)
let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

(*I seperated the from_file function in two because i think it is easier to get the infos using this method*)
let from_file_personnes path =

  let infile = open_in path in
  (* Read all lines until end of file. *)
  let rec loop personnes =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let personnes2 =
        (* Ignore empty lines *)
        if line = "" then personnes

        (* The first character of a line determines its content : p or d. *)
        else match line.[0] with
          | 'p' -> read_personne personnes line
          | 'd' -> personnes

          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment personnes line
      in      
      loop personnes2
    with End_of_file -> personnes (* Done *)
  in

  let final_personnes = loop [] in
  close_in infile ;
  final_personnes

let from_file_depenses path =

  let infile = open_in path in
  (* Read all lines until end of file. *)
  let rec loop2 depenses =
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let depenses2 =
        (* Ignore empty lines *)
        if line = "" then depenses

        (* The first character of a line determines its content : p or d. *)
        else match line.[0] with
          | 'd' -> read_depense depenses line
          | 'p' -> depenses
          (* It should be a comment, otherwise we complain. *)
          | _ -> read_comment depenses line
      in      
      loop2 depenses2
    with End_of_file -> depenses (* Done *)
  in

  let final_depenses = loop2 [] in
  close_in infile ;
  final_depenses


(*Doing the average of an expense*)
let moyenne_depense depense personnes= depense.prix /. float_of_int(List.length personnes)

(*Using the previous average to calculate the average of each persons for all expenses*)
let moyenne_depenses depenses personne personnes= 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> aux t personne (acc +. moyenne_depense depense personnes)
  in aux depenses personne 0.0

(*Adding the exepenses of a personne if he payed it*)
let ajout_paiement depenses personne = 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> if personne.id=depense.personne  then aux t personne (acc +. depense.prix) 
    else aux t personne acc
  in aux depenses personne 0.0

(*Calculating the "solde" of a personne (will be used to create capacity of the source and sink arcs)*)
let solde_personne depenses personne personnes= ajout_paiement depenses personne -. moyenne_depenses depenses personne personnes

(*Creating a dette list which is quite the same as an arc list*)
let calcul_dettes personnes  = 
  let rec aux personnes acc = match personnes with
    | [] -> acc
    | personne::t -> 
      let rec aux2 personne personnes acc = 
      match personnes with
      | [] -> acc
      | personne2::t2 -> if personne.id<>personne2.id then aux2 personne t2({creancier=personne;debiteur=personne2;montant=Float.infinity}::acc)
      else aux2 personne t2 acc
    in aux t (aux2 personne personnes acc)
  in aux personnes []

(*Just a function because I found that some of my dettes are replicating (idk why)*)
let doublons dettes = 
  let rec aux dettes acc = match dettes with
    | [] -> acc
    | dette::t -> if List.mem dette acc then aux t acc else aux t (dette::acc)
  in aux dettes []

(*Creating the dettes list with all the interactions possible between the persons*)
let final_calcul_dettes personnes = doublons(calcul_dettes personnes@calcul_dettes (List.rev(personnes)))

(*Linking the id of a personne with his node in the graph and so creating the node*)
let rec create_nodes personnes graph = 
  Printf.printf "creating_node\n%!" ;
  match personnes with
    | [] -> graph
    | personne::t -> create_nodes t (new_node graph personne.id)

(*Creating the arcs in the graph using the dettes list*)
let rec create_arcs dettes graph = 
  Printf.printf "creating_arc\n%!" ;
  match dettes with
    | [] -> graph
    | dette::t -> create_arcs t (new_arc graph {src= dette.creancier.id;  tgt= dette.debiteur.id; lbl=dette.montant})

(*Final function to create the graph using create_arcs and create_nodes*)
let create_graph dettes personnes graph = 
  let graph = create_nodes personnes graph in
  create_arcs dettes graph

(*I think the name explains it all but just to precise that I used a second list of personnes just to keep it complete for the list.lengh of moyenne_depense*)
let update_solde personnes depenses = 
  let personnes2 = personnes in
  let rec aux personnes acc = match personnes with
    | [] -> acc
    | personne::t -> aux t ({id=personne.id;nom=personne.nom;solde=solde_personne depenses personne personnes2}::acc)
  in aux personnes []

(*It creates the source and the sink and more importantly it links the source with the persons with debts and links the sink with the persons without debts
   so the source node and the sink node will have the capacities of the people's "soldes"*)
let dette_node_and_creance_node graph personnes depenses=
  let graph = new_node graph 0 in
  let graph = new_node graph 99 in
  let personnes = update_solde personnes depenses in
  let rec aux graph personnes = match personnes with
    | [] -> graph
    | personne::t -> if personne.solde<0.0 then aux (new_arc graph {src=0;tgt=personne.id;lbl=(-.personne.solde)}) t
    else aux (new_arc graph {src=personne.id;tgt=99;lbl=personne.solde}) t
  in aux graph personnes

(*Final function which use all the sub functions defined previously and then gives us the float graph we want to have for FF function*)
let flow_remboursement personnes depenses =
  let personnes2 = personnes in
  let rec soldes_personnes personnes depenses = 
    match personnes with
    | [] -> []
    | personne::t -> {id=personne.id;nom=personne.nom;solde=solde_personne depenses personne personnes2}::soldes_personnes t depenses
  in let personnes = soldes_personnes personnes depenses in
  let dettes= final_calcul_dettes personnes in
  let graph = create_graph dettes personnes empty_graph in
  dette_node_and_creance_node graph personnes depenses

(*After having all these functions, I wasn't satisfied with the print of our graph so I decided to modify the export function.
   The conversion function just associate the id of a person with the id of his node in the graph*)
let rec conversion id personnes = match personnes with
  | [] -> failwith "No one"
  | personne::t -> if personne.id=id then personne.nom else conversion id t

(*Using some if to know if it's the source (0) or the sink (99) I have adapted the export version putting my conversion function in so that I can have name on nodes*)
let export_money path graph personnes=

  let ff = open_out path in

  fprintf ff "digraph finite_state_machine {\n" ;
  fprintf ff "rankdir=LR;\n" ;
  fprintf ff "size=\"8,5\"\n" ;
  fprintf ff "node [shape = circle]; " ;
  fprintf ff "Dettes [shape = doublecircle, style=filled, fillcolor=red]\n";
  fprintf ff "Surplus [shape = doublecircle, style=filled, fillcolor=green]\n";
  n_iter_sorted graph (fun id -> if id=0 || id=99 then (if id=0 then fprintf ff "Dettes " else fprintf ff "Surplus ") else fprintf ff "%s " (conversion id personnes)) ;
  fprintf ff ";\n" ;
  e_iter graph (fun arc -> if arc.src=0 || arc.tgt=99 then (if arc.src=0 then fprintf ff "Dettes -> %s [ label = \"%s\" ];\n" (conversion arc.tgt personnes) arc.lbl else fprintf ff "%s -> Surplus [ label = \"%s\" ];\n" (conversion arc.src personnes) arc.lbl) else fprintf ff "%s -> %s [ label = \"%s\" ];\n" (conversion arc.src personnes) (conversion arc.tgt personnes) arc.lbl) ;
  fprintf ff "}\n" ;
  
  close_out ff ;
  ()