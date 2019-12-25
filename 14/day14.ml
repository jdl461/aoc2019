open Core

type chem = {
  name : string;
  amount : int;
}

let make_chem name amount = { name; amount; }
let chem_to_str chem = Printf.sprintf "%s:%d" chem.name chem.amount

let data () = In_channel.read_lines "14/input.txt"

let add_chem t name amount prox = 
  Map.set t ~key:name 
    ~data:(
      Map.set (Map.empty (module Int))
        ~key:amount
        ~data:(
          List.map prox ~f:(fun (name, amt) -> make_chem name amt)
        )
    )

let add_input_to_table d t = 
  let reax = String.split ~on:' ' d 
             |> List.rev 
             |> List.filter ~f:(fun s -> not (String.equal s "=>"))
             |> List.map ~f:(String.rstrip ~drop:(fun c -> (Char.equal c ',')))
             |> List.chunks_of ~length:2
             |> List.map ~f:(
               fun t -> match t with 
                 | [n;a] -> (n,a) 
                 | _ -> failwith "nope"
             )
             |> List.map ~f:(fun (n,a) -> (n, Int.of_string a)) in
  match reax with
  | (n,a) :: tl -> add_chem t n a tl
  | _ -> failwith "bad input"


let table = let init = Map.empty (module String) in
  let d = data () in
  List.fold d ~init:init ~f:(fun acc a -> add_input_to_table a acc)


let fuel_reax = Option.(
    Map.find table "FUEL" >>= 
    fun x -> Map.find x 1 
  )

let () = match fuel_reax with
  | Some reax -> List.iter reax ~f:(fun r -> print_endline (chem_to_str r))
  | None -> failwith "nope"
