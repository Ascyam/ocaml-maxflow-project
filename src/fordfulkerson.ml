open Graph
open Tools

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

(*only for test*)
let print_path path = 
  let rec aux = function
    |[] -> ()
    |x::t -> 
      Printf.printf "%d -> %d (%d/%d)\n" x.src x.tgt x.lbl.acu x.lbl.capa;
      aux t
  in aux path  

(*unused*)
let diff_graph gr = gmap gr (fun label -> {acu = label.capa - label.acu; capa = label.capa})

let string_of_flow flow = "" ^ string_of_int flow.acu ^ "/" ^ string_of_int flow.capa ^ ""

let rec update_flow graphf path acuflow = 
  match path with
  |[] -> graphf
  |x::t -> 
    let new_graphf = add_arc graphf x.src x.tgt {acu= acuflow; capa=x.lbl.capa} in
    update_flow new_graphf t acuflow


let rec flow_min path = 
  match path with
  |[] -> max_int
  |x::t -> 
    if x.lbl.capa - x.lbl.acu < flow_min t then x.lbl.capa - x.lbl.acu
    else flow_min t
  

  let ford_fulkerson graph src dst = 
    let graphf = init_graphf graph in
    let rec aux graphf src dst = 
      let path = find_path graphf src dst in
      if path = [] then graphf
      else
        let flow = flow_min path in
        let new_graphf = update_flow graphf path flow in
        aux new_graphf src dst
    in aux graphf src dst



(*Passage en float*)

let init_graphf_f gr = gmap gr (fun label -> {acuf = 0.0; capaf = label})

let find_path_f graphf src dst = 
  Printf.printf "src : %d\n" src ;
  let rec parcourir graphf src dst marked stock= 
    if src = dst then stock 
    else
      let arcs = out_arcs graphf src in
      let rec aux2 arcliste =
        match arcliste with
        |[] -> []
        |x::t -> 
          if (not (List.mem x.tgt marked)) && (x.lbl.acuf < x.lbl.capaf) then
            let new_path = {src= x.src; tgt = x.tgt; lbl={acuf=x.lbl.acuf; capaf=x.lbl.capaf}}::stock in
            let marked = (x.tgt)::marked in
            let path = (List.rev (parcourir graphf x.tgt dst marked new_path)) in
            if path = [] then aux2 t
            else path
          else aux2 t
      in aux2 arcs
  in parcourir graphf src dst [src] []
;;

let string_of_flow_f flow_f = "" ^ string_of_float flow_f.acuf ^ "/" ^ string_of_float flow_f.capaf ^ ""

let rec update_flow_f graphf path acuflow = 
  match path with
  |[] -> graphf
  |x::t -> 
    let new_graphf = add_arc_f graphf x.src x.tgt {acuf= acuflow; capaf=x.lbl.capaf} in
    update_flow_f new_graphf t acuflow


let rec flow_min_f path = 
  match path with
  |[] -> max_float
  |x::t -> 
    if x.lbl.capaf -. x.lbl.acuf < flow_min_f t then x.lbl.capaf -. x.lbl.acuf
    else flow_min_f t
  

  let ford_fulkerson_f graph src dst = 
    let graphf = init_graphf_f graph in
    let rec aux graphf src dst = 
      let path = find_path_f graphf src dst in
      if path = [] then graphf
      else
        let flow = flow_min_f path in
        let new_graphf = update_flow_f graphf path flow in
        aux new_graphf src dst
    in aux graphf src dst

