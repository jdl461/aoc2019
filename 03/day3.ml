open Core

let input = In_channel.read_lines "03/input.txt"
let paths = List.map ~f:(String.split ~on:',') input
let path1 = List.nth_exn paths 0
let path2 = List.nth_exn paths 1


(* let path1 = ["R8";"U5";"L5";"D3"] *)
(* let path1 = ["R75";"D30";"R83";"U83";"L12";"D49";"R71";"U7";"L72"] *)

(* let path2 = ["U7";"R6";"D4";"L4"] *)
(* let path2 = ["U62";"R66";"U55";"R34";"D71";"R55";"D58";"R83"] *)

let direction path = String.prefix path 1
let distance path dir = String.chop_prefix_exn path ~prefix:dir |> Int.of_string

let point_to_string (x, y) = (Int.to_string x) ^ ":" ^ (Int.to_string y)
let string_to_point s = let splits = String.split ~on:':' s in
  match splits with
  | [a;b] -> (Int.of_string a, Int.of_string b)
  | _ -> failwith "Nope"

let move (x,y) direction distance =
  match direction with 
  | "L" -> List.map (List.range ~stride:(-1) (x) (x - distance - 1)) ~f:(fun x2 -> point_to_string (x2, y))
  | "R" -> List.map (List.range ~stride:(1) x (x + distance + 1)) ~f:(fun x2 -> point_to_string (x2, y))
  | "U" -> List.map (List.range ~stride:(1) y (y + distance + 1)) ~f:(fun y2 -> point_to_string (x, y2))
  | "D" -> List.map (List.range ~stride:(-1) (y) (y - distance - 1)) ~f:(fun y2 -> point_to_string (x, y2))
  | _ -> raise (Invalid_argument("unknown direction " ^ direction))

let hash = Hashtbl.create (module String)

let update_hash h point v = 
  let freq_data = Hashtbl.find h point in
  let newCount = match freq_data with
    | Some c -> c
    | None -> v in
  Hashtbl.set h ~key:point ~data:newCount

let p1 = List.fold path1 ~init:("1:1", 0)
    ~f:(
      fun (point, total) a -> 
        let dir = direction a in
        let dist = distance a dir in
        let points = move (string_to_point point) dir dist in
        let () = List.iteri points ~f:(fun i key -> update_hash hash key (total + i)) in
        (List.last_exn points, total + dist)
    )

(* let () = Hashtbl.iteri hash 
    ~f:(
      fun ~key ~data -> print_endline (key ^ " " ^ Int.to_string data)
    ) *)

let intersections = Hashtbl.create (module String)

let print_intersections h = Hashtbl.iteri h 
    ~f:(
      fun ~key:point ~data:freq -> print_endline (point ^ " " ^ Int.to_string freq)
    )

let update_intersections h point v =
  let freq = Hashtbl.find h point in
  match freq with
  | Some _ -> ()
  | None -> Hashtbl.set h ~key:point ~data:v

let p2 = List.fold path2 ~init:("1:1", 0)
    ~f:(
      fun (point, total) a -> 
        let dir = direction a in
        let dist = distance a dir in
        let points = move (string_to_point point) dir dist in
        let () = List.iteri points 
            ~f:(
              fun i point -> let p = Hashtbl.find hash point in
                match p with
                | Some f -> update_intersections intersections point (total + i + f)
                | None -> ()
            ) in
        (List.last_exn points, total + dist)
    )

let () = print_endline ""
let () = print_intersections intersections