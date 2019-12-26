open Core

type chem = {
  name : string;
  amount : int;
}

let chem_to_str chem = Printf.sprintf "%s:%d" chem.name chem.amount
let chem_list_to_str chems = (Printf.sprintf "%s" (List.to_string chems ~f:chem_to_str))

let data () = In_channel.read_lines "14/input.txt"

let ex1 () = [
  "10 ORE => 10 A";
  "1 ORE => 1 B";
  "7 A, 1 B => 1 C";
  "7 A, 1 C => 1 D";
  "7 A, 1 D => 1 E";
  "7 A, 1 E => 1 FUEL";
]

let ex2 () = [
  "9 ORE => 2 A";
  "8 ORE => 3 B";
  "7 ORE => 5 C";
  "3 A, 4 B => 1 AB";
  "5 B, 7 C => 1 BC";
  "4 C, 1 A => 1 CA";
  "2 AB, 3 BC, 4 CA => 1 FUEL";
]

let ex3 () = [
  "157 ORE => 5 NZVS";
  "165 ORE => 6 DCFZ";
  "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL";
  "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ";
  "179 ORE => 7 PSHF";
  "177 ORE => 5 HKGWZ";
  "7 DCFZ, 7 PSHF => 2 XJWVT";
  "165 ORE => 2 GPVTF";
  "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";
]

let ex4 () = [
  "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG";
  "17 NVRVD, 3 JNWZP => 8 VPVL";
  "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL";
  "22 VJHF, 37 MNCFX => 5 FWMGM";
  "139 ORE => 4 NVRVD";
  "144 ORE => 7 JNWZP";
  "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC";
  "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV";
  "145 ORE => 6 MNCFX";
  "1 NVRVD => 8 CXFTF";
  "1 VJHF, 6 MNCFX => 4 RFSQX";
  "176 ORE => 6 VJHF";
]

let ex5 () = [
  "171 ORE => 8 CNZTR";
  "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL";
  "114 ORE => 4 BHXH";
  "14 VRPVC => 6 BMBT";
  "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL";
  "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT";
  "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW";
  "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW";
  "5 BMBT => 4 WPTQ";
  "189 ORE => 9 KTJDG";
  "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP";
  "12 VRPVC, 27 CNZTR => 2 XDBXC";
  "15 KTJDG, 12 BHXH => 5 XCVML";
  "3 BHXH, 2 VRPVC => 7 MZWV";
  "121 ORE => 7 VRPVC";
  "7 XCVML => 6 RJRHP";
  "5 BHXH, 4 VRPVC => 5 LTCX";
]

let slop = Hashtbl.create (module String)

let get_slop name = 
  Hashtbl.find_or_add slop name ~default:(fun () -> 0)

let add_slop name amt = 
  let a = Hashtbl.find_or_add slop name ~default:(fun () -> 0) in
  Hashtbl.set slop ~key:name ~data:(a + amt)

let take_slop name amt = 
  let a = Hashtbl.find_or_add slop name ~default:(fun () -> 0) in
  Hashtbl.set slop ~key:name ~data:(a - amt)

let make_chem name amount = { name; amount; }

let add_input_to_table d t = 
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

(* true if the chem directly produces ore *)
let is_ore_product chem table = 
  let react = Map.find table chem.name in
  match react with
  | Some (_, products) -> (
      match products with
      | {name = "ORE"; amount = _} :: [] -> true
      | _ -> false
    )
  | None -> false

(* true if all the chems in the list directly produce ore *)
let is_reduced chems table = 
  List.fold chems 
    ~init:true ~f:(
    fun acc a -> acc && (is_ore_product a table)
  )

let increase incr chems = 
  List.map chems ~f:(fun {name;amount} -> {name; amount = amount * incr;})

let calc_incr need have = 
  let incr = Float.round_up (need // have) |> Float.to_int in
  incr

let expand name required table = 
  Option.(
    Map.find table name >>|
    fun (amount, products) -> 
    let xtra = get_slop name in
    let incr = calc_incr (required - xtra) amount in
    let xtra = (incr * amount) - required in
    let () = add_slop name xtra in
    let new_products = increase incr products in
    new_products
  ) |> Option.value ~default:[]

let rec combine_chems chems = 
  let combine terms = List.fold terms ~init:{name="";amount=0} 
      ~f:(fun total chem -> {name = chem.name; amount = total.amount + chem.amount}) in
  match chems with 
  | [] -> []
  | chem :: [] -> [chem]
  | chem :: _ -> 
    let (p1,p2) = List.partition_tf chems ~f:(fun c -> String.equal c.name chem.name) in
    (combine p1) :: combine_chems p2

let step (chems : chem list) table = 
  let chems = combine_chems chems in
  let is_reduced = is_reduced chems table in
  if is_reduced 
  then chems 
  else
    List.fold chems ~init:[]
      ~f:(
        fun acc chem -> 
          if is_ore_product chem table 
          then List.append [chem] acc
          else List.append (expand chem.name chem.amount table) acc
      )

let convert_to_ore chems table = 
  List.map chems ~f:(
    fun c -> expand c.name c.amount table
  ) |> List.join
  |> combine_chems

let table = 
  let init = Map.empty (module String) in
  let d = data () in
  List.fold d ~init:init ~f:(fun acc a -> add_input_to_table a acc)

let calc_ore fuel = 
  let rec run chems table = 
    let is_reduced = is_reduced chems table in
    match is_reduced with
    | true -> convert_to_ore (combine_chems chems) table
    | false -> let s = step chems table in
      run s table in

  let ore = run [make_chem "FUEL" fuel] table in
  match ore with
  | ore :: [] -> ore.amount
  | _ -> failwith "output error"

let p1 = calc_ore 1
let () = print_endline (Int.to_string p1)


let rec f lower upper target (fn : int -> int) =
  let guess = (lower + upper) / 2 in
  let r = fn guess in
  let () = print_endline (Printf.sprintf "Trying %d = %d" guess r) in
  if r = target
  then guess
  else if r < target then f (guess+1) upper target fn
  else f lower (guess-1) target fn


let lower = (1000000000000 / p1)
let upper = lower * 2

let x = f lower upper 1000000000000 calc_ore
let () = print_endline (Int.to_string x)

