open Core

(** part 1 answer 12490719 **)

let input = In_channel.read_all "02/input.txt"
let get_opcodes = Array.map (Array.of_list (String.split ~on:',' input)) ~f:Int.of_string

(* returns the next pc *)
let execute_instruction pc data =
  let opcode = Array.get data pc in
  match opcode with
  | 1 -> let loc = Array.get data (pc + 3) in
    let a = Array.get data (Array.get data (pc + 1)) in
    let b = Array.get data (Array.get data (pc + 2)) in
    let () = Array.set data loc (a + b) in
    pc + 4
  | 2 -> let loc = Array.get data (pc + 3) in
    let a = Array.get data (Array.get data (pc + 1)) in
    let b = Array.get data (Array.get data (pc + 2)) in
    let () = Array.set data loc (a * b) in
    pc + 4
  | 99 -> -1
  | o -> raise (Invalid_argument ("Invalid opcode " ^ (Int.to_string o)))

let rec execute pc data =
  let new_pc = execute_instruction pc data in
  match new_pc with
  | -1 -> Array.get data 0
  | x -> execute x data

let run (noun, verb) =
  let o = Array.map (Array.of_list (String.split ~on:',' input)) ~f:Int.of_string in
  (* let () = print_endline (List.to_string ~f:Int.to_string (Array.to_list o)) in *)
  let () = Array.set o 1 noun in
  let () = Array.set o 2 verb in
  (* let () = print_endline (List.to_string ~f:Int.to_string (Array.to_list o)) in *)
  let result = execute 0 o in
  (* let () = print_endline (List.to_string ~f:Int.to_string (Array.to_list o)) in *)
  let () = print_endline (Int.to_string result ^ " " ^ Int.to_string noun ^ " " ^ Int.to_string verb) in
  match result with
  | 19690720 -> (noun, verb)
  | _ -> (-1, -1)

let nouns = List.range 0 100
let verbs = List.range 0 100

let candidates = List.cartesian_product nouns verbs

let results = List.map candidates ~f:run
let pairs = List.filter results ~f:(fun (x,y) -> x != -1 && y != -1)
let () = List.iter pairs ~f:(fun (n,v) -> print_endline ((Int.to_string n) ^ (Int.to_string v)))
