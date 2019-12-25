open Core

type chem = {
  name : string;
  amount : int;
}

let chem_to_str chem = Printf.sprintf "%s:%d" chem.name chem.amount

let _data () = In_channel.read_lines "14/input.txt"

let data () = [
  "10 ORE => 10 A";
  "1 ORE => 1 B";
  "7 A, 1 B => 1 C";
  "7 A, 1 C => 1 D";
  "7 A, 1 D => 1 E";
  "7 A, 1 E => 1 FUEL";
]

let add_input_to_table d t = 
  let make_chem name amount = { name; amount; } in
  let add_chem t name amount prods = 
    Map.set t ~key:name 
      ~data:(amount, List.map prods ~f:(fun (name,amt) -> make_chem name amt )) in
  let strip_comma s = String.rstrip s ~drop:(fun c -> (Char.equal c ',')) in 
  let reax = String.split ~on:' ' d 
             |> List.rev 
             |> List.filter ~f:(fun s -> not (String.equal s "=>"))
             |> List.map ~f:strip_comma
             |> List.chunks_of ~length:2
             |> List.map ~f:(
               fun t -> match t with 
                 | [name; amount] -> (name, Int.of_string amount) 
                 | _ -> failwith (List.to_string t ~f:ident)
             ) in
  match reax with
  | (name,amt) :: prods -> add_chem t name amt prods
  | _ -> failwith "bad input"

let increase amt chems = 
  List.map chems ~f:(fun {name;amount} -> {name; amount = amount * amt;})

let calc_incr need have = 
  let () = print_endline (Printf.sprintf "have %d need %d" have need) in
  let incr = Float.round_up (need // have) |> Float.to_int in
  let () = print_endline (Printf.sprintf "incr is %d" incr) in
  incr

let expand name required table = 
  let () = print_endline (Printf.sprintf "expanding %s:%d" name required) in
  Option.(
    Map.find table name >>|
    fun (amount, products) -> 
    let incr = calc_incr required amount in
    let new_products = increase incr products in
    let () = print_endline (Printf.sprintf "Need: %s" (List.to_string new_products ~f:chem_to_str) ) in
    new_products
  ) |> Option.value ~default:[]

let table = 
  let init = Map.empty (module String) in
  let d = data () in
  List.fold d ~init:init ~f:(fun acc a -> add_input_to_table a acc)

let combine_terms chems = chems

let rec run chems table = 
  let () = print_endline (Printf.sprintf "Processing: %s" (List.to_string chems ~f:chem_to_str) ) in
  let combined = combine_terms chems in
  let () = print_endline (Printf.sprintf "Combined: %s" (List.to_string combined ~f:chem_to_str) ) in
  match combined with
  | [] -> []
  | {name = "ORE"; _} as ore :: [] -> [ore]
  | {name = "ORE"; _} as ore :: tl -> ore :: run tl table
  | {name; amount} :: [] -> run (expand name amount table) table
  | {name; amount} :: tl -> run (List.append (expand name amount table) tl) table


let fuels = let f = expand "FUEL" 1 table in
  run f table

let () = List.iter fuels ~f:(fun chem -> print_endline (chem_to_str chem))

let total = List.fold fuels ~init:0 ~f:(fun acc a -> a.amount + acc)
let () = print_endline (Printf.sprintf "%d" total)
