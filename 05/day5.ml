open Core

(* part 1 answer 13294380 *)

let input = In_channel.read_all "05/input.txt"
let get_opcodes = Array.append (Array.of_list (String.split ~on:',' input)) (Array.create ~len:5000 "0")

let parse_opcode instruction = 
  match instruction with
  |  "1" -> "01"
  |  "2" -> "02"
  |  "3" -> "03"
  |  "4" -> "04" 
  |  "5" -> "05" 
  |  "6" -> "06" 
  |  "7" -> "07" 
  |  "8" -> "08" 
  | "99" -> "99"
  | _ ->
    let lst = String.to_list instruction in
    match (lst) with
    | [_;_;_;o;p] -> String.of_char_list [o;p]
    |   [_;_;o;p] -> String.of_char_list [o;p]
    |     [_;o;p] -> String.of_char_list [o;p]
    | _ -> failwith ("Invalid opcode " ^ instruction)

let parse_modes instruction =
  let lst = String.to_list instruction in
  match (lst) with
  | [a;b;c;_;_] -> (Char.to_string a, Char.to_string b, Char.to_string c) 
  |   [b;c;_;_] -> ("0", Char.to_string b, Char.to_string c)
  |     [c;_;_] -> ("0", "0", Char.to_string c)
  | _ -> ("0", "0", "0")

let get_param data offset mode = 
  let result = match mode with
    | "0" -> let mem = Int.of_string data.(offset) in
      data.(mem)
    | "1" -> data.(offset)
    | _ -> failwith ("Invalid mode " ^ mode) in
  result

let set_param (data : string array) (offset : int) (param : string) =
  data.(offset) <- param

(* returns the next pc *)
let execute_instruction pc data =
  let instruction =  data.(pc) in
  let opcode = parse_opcode instruction in
  let (_,m2,m1) = parse_modes instruction in
  match opcode with
  | "01" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let () = set_param data loc (Int.to_string (a + b)) in
    pc + 4
  | "02" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let () = set_param data loc (Int.to_string (a * b)) in
    pc + 4
  | "03" ->
    let loc = Int.of_string (get_param data (pc + 1) "1") in
    let () = set_param data loc "5" in
    pc + 2
  | "04" -> 
    let a = get_param data (pc + 1) m1 in
    let () = print_endline ("Output: " ^ a) in
    pc + 2
  | "05" -> 
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    if (not @@ phys_equal a 0) then b else pc + 3
  | "06" ->
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    if (phys_equal a 0) then b else pc + 3
  | "07" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let v = if a < b then "1" else "0" in
    let () = set_param data loc v in
    pc + 4
  | "08" ->
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let v = if (phys_equal a b) then "1" else "0" in
    let () = set_param data loc v in
    pc + 4
  | "99" -> 
    -1
  | o -> raise (Invalid_argument ("Invalid instruction " ^ o))

let rec execute pc data =
  let new_pc = execute_instruction pc data in
  match new_pc with
  | -1 -> data.(0)
  | x -> execute x data

let run = execute 0 get_opcodes
