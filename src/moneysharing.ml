open Graph

type personne = {
  id : int;
  nom : string;
  solde : float;
}

type depense = {
  personne : int;
  prix : float;
  (*participants : personne list;*)
}

type dette = {
  creancier : personne;
  debiteur : personne;
  montant : float;
}

let new_personne personnes id nom solde = {id=id;nom=nom;solde=solde}::personnes

let new_depense depenses payeur prix = {personne=payeur;prix=prix}::depenses

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

let read_comment graph line =
  try Scanf.sscanf line " %%" graph
  with _ ->
    Printf.printf "Unknown line:\n%s\n%!" line ;
    failwith "from_file"

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


let moyenne_depense depense = depense.prix /. float_of_int(5) 

let moyenne_depenses depenses personne = 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> aux t personne (acc +. moyenne_depense depense)
  in aux depenses personne 0.0

let ajout_paiement depenses personne = 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> if personne.id=depense.personne  then aux t personne (acc +. depense.prix) 
    else aux t personne acc
  in aux depenses personne 0.0

let solde_personne depenses personne = ajout_paiement depenses personne -. moyenne_depenses depenses personne

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

let doublons dettes = 
  let rec aux dettes acc = match dettes with
    | [] -> acc
    | dette::t -> if List.mem dette acc then aux t acc else aux t (dette::acc)
  in aux dettes []

let final_calcul_dettes personnes = doublons(calcul_dettes personnes@calcul_dettes (List.rev(personnes)))

(*probablement inutile*)
(*let rec dettes_to_float_arc_list dettes = match dettes with
  | [] -> []
  | dette::t -> {src=dette.creancier.id;tgt=dette.debiteur.id;lbl=dette.montant}::dettes_to_float_arc_list t*)


(*let create_graph dettes personnes graph =
  let rec create_nodes personnes graph = match personnes with
    | [] -> empty_graph
    | personne::t -> create_nodes t (new_node graph personne.id)
  in let rec create_arcs depenses graph = match depenses with
    | [] -> graph
    | dette::t -> create_arcs t (new_arc graph {src= dette.creancier.id;  tgt= dette.debiteur.id; lbl=dette.montant})
  in create_arcs dettes (create_nodes personnes graph)*)

let rec create_nodes personnes graph = 
  Printf.printf "creating_node\n%!" ;
  match personnes with
    | [] -> graph
    | personne::t -> create_nodes t (new_node graph personne.id)

let rec create_arcs dettes graph = 
  Printf.printf "creating_arc\n%!" ;
  match dettes with
    | [] -> graph
    | dette::t -> create_arcs t (new_arc graph {src= dette.creancier.id;  tgt= dette.debiteur.id; lbl=dette.montant})

let create_graph dettes personnes graph = 
  Printf.printf "creating_graph\n%!" ;
  let graph = create_nodes personnes graph in
  Printf.printf "graph: %s\n%!" ("Fin création noeuds");
  create_arcs dettes graph

let update_solde personnes depenses = 
  let rec aux personnes acc = match personnes with
    | [] -> acc
    | personne::t -> aux t ({id=personne.id;nom=personne.nom;solde=solde_personne depenses personne}::acc)
  in aux personnes []

let dette_node_and_creance_node graph personnes depenses=
  let graph = new_node graph 0 in
  let graph = new_node graph 99 in
  let personnes = update_solde personnes depenses in
  let rec aux graph personnes = match personnes with
    | [] -> graph
    | personne::t -> if personne.solde<0.0 then aux (new_arc graph {src=0;tgt=personne.id;lbl=(-.personne.solde)}) t
    else aux (new_arc graph {src=personne.id;tgt=99;lbl=personne.solde}) t
  in aux graph personnes

let flow_remboursement personnes depenses =
  Printf.printf "personnes: %s\n%!" (List.fold_left (fun acc personne -> acc ^ " " ^ personne.nom) "" personnes) ;
  Printf.printf "depenses: %s\n%!" (List.fold_left (fun acc depense -> acc ^ " " ^ string_of_float depense.prix) "" depenses) ;
  let rec soldes_personnes personnes depenses = 
    match personnes with
    | [] -> []
    | personne::t -> {id=personne.id;nom=personne.nom;solde=solde_personne depenses personne}::soldes_personnes t depenses
  in let personnes = soldes_personnes personnes depenses in
  let dettes= final_calcul_dettes personnes in
  let graph = create_graph dettes personnes empty_graph in
  Printf.printf "graph: %s\n%!" ("Fin fr") ;
  dette_node_and_creance_node graph personnes depenses


(*let alan = {id=1;nom="Alan";solde=0.0}
let ben = {id=2;nom="Ben";solde=0.0}
let camille = {id=3;nom="Camille";solde=0.0}
let alan2 = {id=4;nom="Alan2";solde=0.0}
let ben2 = {id=5;nom="Ben2";solde=0.0}

let depense1 = {personne=1;prix=10.0}
let depense2 = {personne=2;prix=20.0}
let depense3 = {personne=3;prix=30.0}

let liste_depenses = [depense1;depense2;depense3]
let liste_personnes = [alan;ben;camille;alan2;ben2]

let liste_dettes = calcul_dettes liste_personnes*)