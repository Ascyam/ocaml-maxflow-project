open Graph
open Tools
open Fordfulkerson

type depense = {
  personne : string;
  montant : int;
  participants : string list;
}

type dette = {
  creancier : string;
  debiteur : string;
  montant : int;
}