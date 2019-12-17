open Core

(* note that in order to preserve certain order 
   and also show the conciseness of the implementation, 
   no tail-recursive is used *)
let ins_all_positions x l =
  let rec aux prev acc = function
    | [] -> (prev @ [x]) :: acc |> List.rev
    | hd::tl as l -> aux (prev @ [hd]) ((prev @ [x] @ l) :: acc) tl
  in
  aux [] [] l

let rec permutations = function
  | [] -> []
  | x::[] -> [[x]] (* we must specify this edge case *)
  | x::xs -> List.fold ~f:(fun acc p -> acc @ ins_all_positions x p ) ~init:[] (permutations xs)
