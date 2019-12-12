open Core

let dim data = 
  let rows = String.split data ~on:'\n' in
  match rows with
  | hd :: _ -> (String.length hd, (List.length rows))
  | _ -> failwith "Invalid input"



let data = {|.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##|}

let data = In_channel.read_all "10/input.txt"

let ne a b = not (phys_equal a b)

let (w,h) = dim data

let flatmap = String.concat (String.split data ~on:'\n')

let coords = List.mapi (String.to_list flatmap) 
    ~f:(
      fun i c -> 
        match c with
        | '#' -> (i - (i/w * w), i/w)
        | _ -> (-1,-1)
    )

let asteroids = List.filter coords ~f:(fun (x,y) -> (ne x (-1)) && (ne y (-1)))

let los (x1,y1) asteroids = List.map asteroids 
    ~f:(fun (x2,y2) -> 
        Float.atan2 ((Float.of_int x1) -. (Float.of_int x2)) ((Float.of_int y1) -. (Float.of_int y2)))

let best = List.map asteroids 
    ~f:(
      fun a -> (a, los a (List.filter asteroids 
                            ~f:(fun x -> not (Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal x a)))
                   |> List.dedup_and_sort ~compare:Float.compare)
    )
           |> List.max_elt ~compare:(fun (_,l1) (_,l2) -> Int.compare (List.length l1) (List.length l2) )

let () = match best with
  | Some ((x,y), asteroids) -> print_endline (Printf.sprintf "%d,%d %d" x y (List.length asteroids))
  | _ -> failwith "Jesus"
