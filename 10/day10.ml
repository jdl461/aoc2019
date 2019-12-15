open Core

let data = In_channel.read_all "10/input.txt"

let ne a b = not (phys_equal a b)


let coords = 
  let dim data = 
    let rows = String.split data ~on:'\n' in
    match rows with
    | hd :: _ -> (String.length hd, (List.length rows))
    | _ -> failwith "Invalid input" in
  let flatmap = String.concat (String.split data ~on:'\n') in
  let (w,_) = dim data
  in List.mapi (String.to_list flatmap) 
    ~f:(
      fun i c -> 
        match c with
        | '#' -> (i - (i/w * w), i/w)
        | _ -> (-1,-1)
    )

let asteroids = List.filter coords  
    ~f:(fun a -> (Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare a (-1,-1)) != 0)

let part1 () = 
  let los (x1, y1) asteroids = List.map asteroids 
      ~f:(fun (x2,y2) -> 
          ((x2,y2), Float.atan2 ((Float.of_int x1) -. (Float.of_int x2)) ((Float.of_int y1) -. (Float.of_int y2)))
        ) in
  let best = List.map asteroids 
      ~f:(
        fun a -> 
          (a, los a (List.filter asteroids 
                       ~f:(fun x -> not (Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal x a)))
              |> List.dedup_and_sort ~compare:(fun (_,b) (_,d) -> Float.compare b d))
      )
             |> List.max_elt ~compare:(fun (_,l1) (_,l2) -> Int.compare (List.length l1) (List.length l2) ) in
  match best with
  | Some ((x,y), asteroids) -> print_endline (Printf.sprintf "%d,%d %d" x y (List.length asteroids))
  | _ -> failwith "nope" 

let part2 () = 
  let base = (30,34) in

  let angle_btwn (x1,y1) (x2,y2) = 
    Float.atan2 ((Float.of_int x1) -. (Float.of_int x2)) ((Float.of_int y1) -. (Float.of_int y2)) in

  let dist (x1,y1) (x2,y2) = 
    let xd = (Float.of_int x2) -. (Float.of_int x1) in
    let yd = (Float.of_int y2) -. (Float.of_int y1) in
    sqrt (xd *. xd) +. (yd *. yd) in

  let add_angle angles angle point = 
    let points = Hashtbl.find_or_add angles angle ~default:(fun () -> []) in
    let points = (List.append points [point]) 
                 |> List.sort ~compare:(fun a b -> Float.compare (dist a base) (dist b base)) in
    Hashtbl.set angles ~key:angle ~data:points in

  let asteroids = List.filter asteroids 
      ~f:(fun a -> (Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare a base) != 0) in

  let angles = Hashtbl.create (module Float) in

  let entry_to_string k v = 
    (Printf.sprintf "%f %s" k (List.to_string v
                                 ~f:(fun (x,y) -> Printf.sprintf "%d,%d" x y))) in

  let () = List.iter asteroids 
      ~f:(
        fun point -> let angle = (angle_btwn base point) *. 57.2958 in
          add_angle angles angle point
      ) in

  let (p1,p2) = Hashtbl.keys angles 
                |> List.sort ~compare:(fun a b -> (Float.compare a b))
                |> List.partition_tf ~f:(fun x -> Float.(x <= 0. && x > (-180.))) in

  let keys_ordered = List.append 
      (List.sort p1 ~compare:(fun a b -> (Float.compare a b) * -1))
      (List.sort p2 ~compare:(fun a b -> (Float.compare a b) * -1)) in

  List.iter keys_ordered 
    ~f:(fun k -> let v = Hashtbl.find angles k in
         match v with
         | Some vl -> print_endline (entry_to_string k vl)
         | None -> failwith "nope"
       )

let () = part1 ()
let () = part2 ()