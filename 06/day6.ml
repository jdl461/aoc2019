open Core

let orbits = Hashtbl.create (module String)

let setup pair o = let s = String.split pair ~on:')' in
  match s with
  | [orbitee;orbiter] -> let () = Hashtbl.set orbits ~key:orbiter ~data:orbitee in
    let s = Set.add o orbiter in
    Set.add s orbitee
  | _ -> failwith "Invalid orbit"

let rec path start = 
  let next = Hashtbl.find orbits start in
  match next with
  | Some next -> 
    List.append [start] (path next)
  | None -> []

let rec traverse count start stop =
  let next = Hashtbl.find orbits start in
  match next with
  | Some next -> 
    if (phys_equal next stop) then count else
      traverse (count + 1) next stop
  | None -> count

let input = In_channel.read_lines "06/input.txt" 

let objects = List.fold input ~init:(Set.empty (module String)) 
    ~f:(
      fun acc a -> setup a acc
    )

let t count stop = fun s -> traverse count s stop

let count = List.map (Set.to_list objects) ~f:(t 0 "") |> List.sum (module Int) ~f:ident

let san = Hashtbl.find_exn orbits "SAN"
let me = Hashtbl.find_exn orbits "YOU"

let santa_path = path san
let my_path = path me

let common = List.fold_until ~init:"" my_path 
    ~finish:(fun _ -> "")
    ~f:(
      fun _ a -> 
        let t = List.find santa_path ~f:(fun x -> phys_equal x a) in
        match t with
        | Some o -> Stop(o)
        | None -> Continue("")
    )

let c1 = traverse 0 me common
let c2 = traverse 0 san common

let () = print_endline (Int.to_string (c1 + c2))