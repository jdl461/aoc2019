open Core

(* let data = In_channel.read_all "07/input.txt" *)
let data = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
let get_instructions = Array.append (Array.of_list (String.split ~on:',' data)) (Array.create ~len:5000 "0")

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
  match mode with
  | "0" -> let mem = Int.of_string data.(offset) in
    data.(mem)
  | "1" -> data.(offset)
  | _ -> failwith ("Invalid mode " ^ mode)

let set_param (data : string array) (offset : int) (param : string) =
  data.(offset) <- param

(* returns the next pc *)
let execute_instruction pc data input_stack output_stack =
  let instruction =  data.(pc) in
  let opcode = parse_opcode instruction in
  let (_,m2,m1) = parse_modes instruction in
  match opcode with
  | "01" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let () = set_param data loc (Int.to_string (a + b)) in
    (pc + 4, input_stack, output_stack)
  | "02" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let () = set_param data loc (Int.to_string (a * b)) in
    (pc + 4, input_stack, output_stack)
  | "03" ->
    let input = List.hd_exn input_stack in
    let input_stack = List.drop input_stack 1 in
    let loc = Int.of_string (get_param data (pc + 1) "1") in
    let () = set_param data loc input in
    (pc + 2, input_stack, output_stack)
  | "04" -> 
    let a = get_param data (pc + 1) m1 in
    let output_stack = List.append output_stack [a] in
    (pc + 2, input_stack, output_stack)
  | "05" -> 
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    if (not @@ phys_equal a 0) then (b, input_stack, output_stack) else (pc + 3, input_stack, output_stack)
  | "06" ->
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    if (phys_equal a 0) then (b, input_stack, output_stack) else (pc + 3, input_stack, output_stack)
  | "07" -> 
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let v = if a < b then "1" else "0" in
    let () = set_param data loc v in
    (pc + 4, input_stack, output_stack)
  | "08" ->
    let loc = Int.of_string (get_param data (pc + 3) "1") in
    let a = Int.of_string (get_param data (pc + 1) m1) in
    let b = Int.of_string (get_param data (pc + 2) m2) in
    let v = if (phys_equal a b) then "1" else "0" in
    let () = set_param data loc v in
    (pc + 4, input_stack, output_stack)
  | "99" -> 
    (-1, input_stack, output_stack)
  | _ -> raise (Invalid_argument ("Invalid instruction " ^ opcode))


let rec execute pc data input_stack output_stack =
  let (new_pc, new_input_stack, new_output_stack) = execute_instruction pc data input_stack output_stack in
  match new_pc with
  | -1 -> new_output_stack
  | x -> execute x data new_input_stack new_output_stack


let run_sequence phases =
  List.fold phases ~init:"0" 
    ~f:(
      fun acc a -> let output = execute 0 get_instructions [a; acc] [] in
        List.last_exn output
    )

let output = run_sequence ["1"; "0"; "4"; "3"; "2"]
let () = print_endline output
