open Graph
open Tools


type capacite = int
type flow = int 

type fcarc = (flow * capacite)

type path = (id * id * fcarc) list

type graphf = fcarc graph

let list_arcs arcs = 
  let rec aux = function
    |[] -> []
    |x::t -> (x.tgt, (0, x.lbl))::(aux t)
  in aux arcs

let find_path graphf src dst = 
  let rec parcourir graphf src dst marked stock= 
    if src = dst then stock 
    else
      let arcs = out_arcs graphf src in
      let listf = list_arcs arcs in
      let rec aux2 arcliste =
        match arcliste with
        |[] -> []
        |(id, (flow, capacite))::t -> 
          if (not (List.mem id marked)) && (flow < capacite) then
            let new_path = (src, id, (flow, capacite))::stock in
            let marked = id::marked in
            let path = (List.rev (parcourir graphf id dst marked new_path)) in
            if path = [] then aux2 t
            else path
          else aux2 t
      in aux2 listf
  in parcourir graphf src dst [src] []
;;

let print_path path = 
  let rec aux = function
    |[] -> ()
    |(src, dst, (flow, capacite))::t -> 
      Printf.printf "%d -> %d (%d/%d)\n" src dst flow capacite;
      aux t
  in aux path  

let init_flow gr = gmap gr (fun x -> (0,x)) 

let diff_graph gr = gmap gr (fun (flow, capacite)-> capacite-flow)


let rec min_capa graph src acu = function
  | [] -> acu
  | id :: rest -> 
    let arc = find_arc graph src id in
    match arc with 
    | None -> 0 
    | Some arc -> min_capa graph id (min acu arc.lbl) rest  

let rec update_capa graph path f = 
  match path with
  |[] -> graph
  |(src, dst, (flow, capacite))::t -> update_capa (add_arc graph src dst (capacite-f)) t flow

let rec path_flow = function
  |[] -> failwith "path_flow"
  |(_, _, (flow, _))::_ -> flow 

let rec path_capacite = function 
  |[] -> failwith "path_capacite"
  |(_, _, (_, capacite))::_ -> capacite
  
