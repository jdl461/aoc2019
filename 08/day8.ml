open Core

let width = 25
let height = 6

let data = In_channel.read_all "08/input.txt"

let count_layer_digits layer digit = List.count layer ~f:(fun d -> (phys_equal d digit ))

let layers = List.chunks_of (String.to_list data) ~length:(width * height)

let part1 () =
  let counts = List.mapi layers ~f:(fun idx layer -> (idx, count_layer_digits layer '0')) in

  let min = List.min_elt counts ~compare:(fun (_,count1) (_, count2) -> Int.compare count1 count2) in

  let idx = match min with
    | Some (idx,_) -> idx
    | None -> failwith "Error" in

  let ones = count_layer_digits (List.nth_exn layers idx) '1' in
  let twos = count_layer_digits (List.nth_exn layers idx) '2' in

  let answer = ones * twos in
  print_endline (Printf.sprintf "%d" answer)


let part2 () = 
  let overlay oldpixel newpixel = 
    if (phys_equal oldpixel '2')
    then newpixel
    else oldpixel in

  let overlay_frame oldframe newframe =
    List.map2_exn oldframe newframe ~f:overlay in

  let image = List.fold (List.drop layers 1) ~init:(List.nth_exn layers 0) 
      ~f:(fun acc a -> overlay_frame acc a) in

  let rows = List.chunks_of image ~length:width in

  List.iter rows 
    ~f:(
      fun row -> let r = String.tr (String.of_char_list row) ~target:'0' ~replacement:' ' in print_endline r
    )

let () = part1 ()
let () = part2 ()
