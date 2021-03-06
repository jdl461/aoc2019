open Core

let min = 168630
let max = 718098


let rle n = 
  let encode s = String.fold s ~init:[]
      ~f:(fun acc elt -> 
          if List.is_empty acc 
          then [(elt, 1)]
          else let (el, cn) = List.last_exn acc in
            if el == elt then 
              let a = List.drop_last_exn acc in
              List.append a [(el, cn + 1)]
            else List.append acc [elt, 1]
        ) in
  let s = Int.to_string n in
  encode s

let is_sorted n = 
  let s = Int.to_string n in
  String.to_list s |> List.is_sorted ~compare:Char.compare

let is_weird n = 
  let e = rle n in
  let f = List.filter e ~f:(fun (e,c) -> c = 2) in
  List.length f > 0

let candidates = List.range ~stride:1 ~start:`inclusive ~stop:`inclusive min max
let count = List.filter candidates
    ~f:(
      fun n -> is_weird n && is_sorted n
    ) |> List.length

let () = count |> Int.to_string |> print_endline

