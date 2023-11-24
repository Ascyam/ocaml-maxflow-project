(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr : 'a graph) = n_fold gr (new_node) empty_graph
let gmap gr f = e_fold gr (fun x y ->new_arc x {src=y.src;tgt=y.tgt;lbl=(f y.lbl)}) (clone_nodes gr)
let add_arc gr s d l= match find_arc gr s d with
  |None -> new_arc gr {src= s; tgt = d; lbl=l}
  |Some x-> new_arc gr {src= s; tgt = d; lbl= l + x.lbl}