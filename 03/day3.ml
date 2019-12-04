open Core

let input = In_channel.read_lines "03/input.txt"
let paths = List.map ~f:(String.split ~on:',') input

let direction path = String.prefix path 1
let distance path dir = String.chop_prefix_exn path ~prefix:dir |> Int.of_string

let manhattan_distance (x1,y1) (x2,y2) = abs(x1 - x2) + abs(y1 - y2)

let move (x1,y1) direction distance =
  match direction with
  | "L" -> (x1 - distance, y1)
  | "R" -> (x1 + distance, y1)
  | "U" -> (x1, y1 + distance)
  | "D" -> (x1, y1 - distance)
  | _ -> raise (Invalid_argument(direction))


let p1 = ["R8";"U5";"L5";"D3"]
let p2 = ["U7";"R6";"D4";"L4"]

let r = List.fold p1 ~init:[(1,1)] 
    ~f:(fun acc segment -> 
        let dir = direction segment in
        let dist = distance segment dir in
        let point = move (List.last_exn acc) dir dist in
        List.append acc [point]
      )

let r2 = List.fold p2 ~init:[(1,1)] 
    ~f:(fun acc segment -> 
        let dir = direction segment in
        let dist = distance segment dir in
        let point = move (List.last_exn acc) dir dist in
        List.append acc [point]
      )
let () = print_endline (List.to_string ~f:(fun (x,y) -> (Int.to_string x) ^ "," ^ (Int.to_string y)) r)
let () = print_endline (List.to_string ~f:(fun (x,y) -> (Int.to_string x) ^ "," ^ (Int.to_string y)) r2)