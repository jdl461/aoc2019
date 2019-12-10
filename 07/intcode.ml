open Core


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


let instruction_3 (amp_state : Ampstate.amp_state) : Ampstate.amp_state = 
  let input = List.hd amp_state.input_stack in
  match input with
  | Some i ->
    (* let () = print_endline ("Got input " ^ i) in *)
    let input_stack = List.drop amp_state.input_stack 1 in
    let loc = Int.of_string (get_param amp_state.data (amp_state.pc + 1) "1") in
    let () = set_param amp_state.data loc i in
    let new_pc = amp_state.pc + 2 in
    { amp_state with 
      pc = new_pc; 
      input_stack = input_stack;
      exit_code = 0; 
    } 
  | None -> 
    (* let () = print_endline ("No input, pausing") in *)
    { amp_state with exit_code = 1 }


let execute_instruction (amp_state : Ampstate.amp_state) : Ampstate.amp_state =
  let instruction =  amp_state.data.(amp_state.pc) in
  let opcode = parse_opcode instruction in
  let (_,m2,m1) = parse_modes instruction in
  match opcode with
  | "01" -> 
    let loc = Int.of_string (get_param amp_state.data (amp_state.pc + 3) "1") in
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    let () = set_param amp_state.data loc (Int.to_string (a + b)) in
    { amp_state with pc = amp_state.pc + 4; exit_code = 0 }
  | "02" -> 
    let loc = Int.of_string (get_param amp_state.data (amp_state.pc + 3) "1") in
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    let () = set_param amp_state.data loc (Int.to_string (a * b)) in
    {amp_state with pc = amp_state.pc + 4; exit_code = 0 }
  | "03" -> instruction_3 amp_state
  | "04" -> 
    let a = get_param amp_state.data (amp_state.pc + 1) m1 in
    (* let () = print_endline ("Output: " ^ a) in *)
    let output_stack = List.append amp_state.output_stack [a] in
    { amp_state with pc = amp_state.pc + 2 ; output_stack = output_stack ; exit_code = 0}
  | "05" -> 
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    if (not @@ phys_equal a 0) 
    then { amp_state with pc = b ; exit_code = 0 }
    else { amp_state with pc = amp_state.pc + 3; exit_code = 0}
  | "06" ->
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    if (phys_equal a 0) 
    then { amp_state with pc = b; exit_code = 0}
    else { amp_state with pc = amp_state.pc + 3; exit_code = 0}
  | "07" -> 
    let loc = Int.of_string (get_param amp_state.data (amp_state.pc + 3) "1") in
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    let v = if a < b then "1" else "0" in
    let () = set_param amp_state.data loc v in
    { amp_state with pc = amp_state.pc + 4; exit_code = 0 }
  | "08" ->
    let loc = Int.of_string (get_param amp_state.data (amp_state.pc + 3) "1") in
    let a = Int.of_string (get_param amp_state.data (amp_state.pc + 1) m1) in
    let b = Int.of_string (get_param amp_state.data (amp_state.pc + 2) m2) in
    let v = if (phys_equal a b) then "1" else "0" in
    let () = set_param amp_state.data loc v in
    { amp_state with pc = amp_state.pc + 4; exit_code = 0}
  | "99" -> 
    { amp_state with pc = -1; exit_code = -1}
  | _ -> raise (Invalid_argument ("Invalid instruction " ^ opcode))
