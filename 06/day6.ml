open Core

let orbits = Hashtbl.create (module String)

let setup pair o = let s = String.split pair ~on:')' in
  match s with
  | [orbitee;orbiter] -> let () = Hashtbl.set orbits ~key:orbiter ~data:orbitee in
    let s = Set.add o orbiter in
    let () = print_endline @@ Printf.sprintf "Adding %s ) %s" orbiter orbitee in
    Set.add s orbitee
  | _ -> failwith "Invalid orbit"


let rec traverse count start =
  let next = Hashtbl.find orbits start in
  match next with
  | Some next -> let () = print_endline (Printf.sprintf "Going from %s to %s" start next) in 
    traverse (count + 1) next
  | None -> count

let input = In_channel.read_lines "06/input.txt" 

let objects = List.fold input ~init:(Set.empty (module String)) 
    ~f:(
      fun acc a -> setup a acc
    )

let () = print_endline @@ "objects " ^ (Int.to_string (Set.length objects))
let () = print_endline @@ "orbits " ^ (Int.to_string (Hashtbl.length orbits))

let count = List.map (Set.to_list objects) ~f:(traverse 0) |> List.sum (module Int) ~f:ident

let () = print_endline (Printf.sprintf "%d" count)