open Tools
open Graph

type capacite = int
type flow = int 

type fcarc = flow*capacite

type path = id list


let init_flow gr = gmap gr (fun x -> (0,x))  



(*let find_path gr path s t*)
