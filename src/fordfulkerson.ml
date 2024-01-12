open Graph
open Tools

type flow =
  {
    acu: int;
    capa: int
  }

let init_graphf gr = gmap gr (fun label -> {acu = 0; capa = label})


let find_path graphf src dst = 
  let rec parcourir graphf src dst marked stock= 
    if src = dst then stock 
    else
      let arcs = out_arcs graphf src in
      let rec aux2 arcliste =
        match arcliste with
        |[] -> []
        |x::t -> 
          if (not (List.mem x.tgt marked)) && (x.lbl.acu < x.lbl.capa) then
            let new_path = {src= x.src; tgt = x.tgt; lbl={acu=x.lbl.acu; capa=x.lbl.capa}}::stock in
            let marked = (x.tgt)::marked in
            let path = (List.rev (parcourir graphf x.tgt dst marked new_path)) in
            if path = [] then aux2 t
            else path
          else aux2 t
      in aux2 arcs
  in parcourir graphf src dst [src] []
;;

let print_path path = 
  let rec aux = function
    |[] -> ()
    |x::t -> 
      Printf.printf "%d -> %d (%d/%d)\n" x.src x.tgt x.lbl.acu x.lbl.capa;
      aux t
  in aux path  

(*let diff_graph gr = gmap gr (fun (flow, capacite)-> capacite-flow)


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

let path_flow = function
  |Some path -> List.fold_left (fun acc (_, _, (flow, _)) -> acc + flow) 0 path
  |None -> 0

let path_capacite = function 
  |Some path -> List.fold_left (fun acc (_, _, (_, capacite)) -> acc + capacite) 0 path
  |None -> 0*)
  