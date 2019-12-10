open Core

let width = 25
let height = 6

let data = In_channel.read_all "08/input.txt"

let count_layer_digits layer digit = List.count layer ~f:(fun d -> (phys_equal d digit ))

let part1 () =
  let layers = List.chunks_of (String.to_list data) ~length:(width * height) in

  let counts = List.mapi layers ~f:(fun idx layer -> (idx, count_layer_digits layer '0')) in

  let min = List.min_elt counts ~compare:(fun (_,count1) (_, count2) -> Int.compare count1 count2) in

  let idx = match min with
    | Some (idx,_) -> idx
    | None -> failwith "Error" in

  let ones = count_layer_digits (List.nth_exn layers idx) '1' in
  let twos = count_layer_digits (List.nth_exn layers idx) '2' in

  let answer = ones * twos in
  print_endline (Printf.sprintf "%d" answer)

let () = part1 ()