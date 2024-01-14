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

val moyenne_depense : depense -> float

val moyenne_depenses: depense list -> personne -> float

val ajout_paiement : depense list -> personne -> float

val solde_personne : depense list -> personne -> float

val calcul_dettes : personne list -> dette list

val create_graph : dette list -> personne list -> float graph -> float graph

val dette_node_and_creance_node : float graph -> personne list -> depense list -> float graph

val flow_remboursement : personne list -> depense list -> float graph

val from_file_personnes : string -> personne list

val from_file_depenses : string -> depense list