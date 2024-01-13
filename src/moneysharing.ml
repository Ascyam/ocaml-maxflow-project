open Graph
open Tools
open Fordfulkerson

type personne = {
  id : int;
  nom : string;
  solde : float;
}

type depense = {
  personne : int;
  prix : float;
  participants : personne list;
}

type dette = {
  creancier : personne;
  debiteur : personne;
  montant : float;
}

let moyenne_depense depense = depense.prix /. float_of_int(List.length depense.participants) 

let add_personne personne liste = match liste with
  | [] -> [personne]
  | _ -> if List.mem personne liste then liste else personne::liste

let add_depense depense liste = match liste with
  | [] -> [depense]
  | _ -> if List.mem depense liste then liste else depense::liste

(*Je ne sais pas si les deux fonctions précedentes seront reellement utiles car je vais probablement écrire les personnes et les dépenses à la main par faute de temps*)

let moyenne_depenses depenses personne = 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> if (List.mem personne depense.participants) && personne.id<>depense.personne  then aux t personne (acc -. moyenne_depense depense) 
    else aux t personne acc
  in aux depenses personne 0.0

let ajout_paiement depenses personne = 
  let rec aux depenses personne acc = match depenses with
    | [] -> acc
    | depense::t -> if (List.mem personne depense.participants) && personne.id=depense.personne  then aux t personne (acc +. depense.prix) 
    else aux t personne acc
  in aux depenses personne 0.0

let solde_personne depenses personne = moyenne_depenses depenses personne +. ajout_paiement depenses personne

let calcul_dettes depenses personnes = 
  let rec aux depenses personnes acc = match personnes with
    | [] -> acc
    | personne::t -> 
      let rec aux2 depenses personne personnes acc = match personnes with
      | [] -> acc
      | personne2::t2 -> if personne.id<>personne2.id then aux2 depenses personne t2 ({creancier=personne;debiteur=personne2;montant=(solde_personne depenses personne -. solde_personne depenses personne2)}::acc)
      else aux2 depenses personne t2 acc
    in aux depenses t (aux2 depenses personne personnes acc)
  in aux depenses personnes []

let rec dettes_to_float_arc_list dettes = match dettes with
  | [] -> []
  | dette::t -> {src=dette.creancier.id;tgt=dette.debiteur.id;lbl=dette.montant}::dettes_to_float_arc_list t
