(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr : 'a graph) = Graph.empty_graph; Graph.n_iter gr 
let gmap gr f = assert false