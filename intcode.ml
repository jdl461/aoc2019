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
  |  "9" -> "09"
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
  |   [b;c;_;_] -> ("1", Char.to_string b, Char.to_string c)
  |     [c;_;_] -> ("1", "0", Char.to_string c)
  | _ -> ("1", "0", "0")


let get_param (amp_state : Ampstate.amp_state) offset mode = 
  let data = amp_state.data in
  let lookup = fun x -> Hashtbl.find_or_add data x ~default:(fun () -> "0") in
  let base = Bigint.of_int amp_state.relative_base in
  let offset =  Bigint.(+) amp_state.pc (Bigint.of_int offset) in
  match mode with
  | "0" -> 
    let mem = Bigint.of_string (lookup offset) in
    lookup mem
  | "1" -> lookup offset
  | "2" -> 
    let rel = Bigint.of_string (lookup offset) in
    let open Bigint in lookup (rel + base)
  | _ -> failwith ("Invalid mode " ^ mode)


let set_param (data : (Bigint.t, string) Hashtbl.t) (offset : Bigint.t) (param : string) =
  Hashtbl.set data ~key:offset ~data:param


let instruction_3 (amp_state : Ampstate.amp_state) mode : Ampstate.amp_state = 
  let input = List.hd amp_state.input_stack in
  match input with
  | Some i ->
    let input_stack = List.drop amp_state.input_stack 1 in
    let loc = Bigint.of_string (get_param amp_state 1 "1") in
    let () = 
      if (String.equal "2" mode)
      then let x = Bigint.(+) loc (Bigint.of_int amp_state.relative_base) in 
        Hashtbl.set amp_state.data ~key:x ~data:i
      else 
        Hashtbl.set amp_state.data ~key:loc ~data:i in
    let new_pc = Bigint.(+) amp_state.pc (Bigint.of_int 2) in
    { amp_state with 
      pc = new_pc; 
      input_stack = input_stack;
      exit_code = 0; 
    } 
  | None -> 
    let () = print_endline ("No input, pausing") in
    { amp_state with exit_code = 1 }


let execute_instruction (amp_state : Ampstate.amp_state) : Ampstate.amp_state =
  let instruction =  Hashtbl.find_or_add amp_state.data (amp_state.pc) ~default:(fun () -> "0") in
  let opcode = parse_opcode instruction in
  let (m3,m2,m1) = parse_modes instruction in
  match opcode with
  | "01" -> 
    let loc = if (String.equal m3 "2") 
      then Bigint.((Bigint.of_string (get_param amp_state (3) "1")) + (Bigint.of_int amp_state.relative_base))
      else Bigint.of_string (get_param amp_state (3) "1") in
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    let v = Bigint.to_string Bigint.(a + b) in
    let () = set_param amp_state.data loc v in
    { amp_state with pc = Bigint.(amp_state.pc + (Bigint.of_int 4)); exit_code = 0 }
  | "02" -> 
    let loc = if (String.equal m3 "2") 
      then Bigint.((Bigint.of_string (get_param amp_state (3) "1")) + (Bigint.of_int amp_state.relative_base))
      else Bigint.of_string (get_param amp_state (3) "1") in
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    let v = (Bigint.to_string Bigint.(a * b)) in
    let () = set_param amp_state.data loc v in
    {amp_state with pc = Bigint.(amp_state.pc + (Bigint.of_int 4)); exit_code = 0 }
  | "03" -> instruction_3 amp_state m1
  | "04" -> 
    let a = get_param amp_state (1) m1 in
    let output_stack = List.append amp_state.output_stack [a] in
    { 
      amp_state with 
      pc = (Bigint.(+) amp_state.pc (Bigint.of_int 2)) ; 
      output_stack = output_stack ; 
      exit_code = 0
    }
  | "05" -> 
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    if (not @@ Bigint.equal a (Bigint.of_int 0)) 
    then { amp_state with pc = b ; exit_code = 0 }
    else { amp_state with pc = (Bigint.(+) amp_state.pc (Bigint.of_int 3)); exit_code = 0}
  | "06" ->
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    if (Bigint.equal a (Bigint.of_int 0)) 
    then { amp_state with pc = b; exit_code = 0}
    else { amp_state with pc = (Bigint.(+) amp_state.pc (Bigint.of_int 3)); exit_code = 0}
  | "07" -> 
    let loc = if (String.equal m3 "2") 
      then Bigint.((Bigint.of_string (get_param amp_state (3) "1")) + (Bigint.of_int amp_state.relative_base))
      else Bigint.of_string (get_param amp_state (3) "1") in
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    let v = let open Bigint in if (a < b) then "1" else "0" in
    let () = set_param amp_state.data loc v in
    { amp_state with pc = (Bigint.(+) amp_state.pc (Bigint.of_int 4)); exit_code = 0 }
  | "08" ->
    let loc = if (String.equal m3 "2") 
      then Bigint.((Bigint.of_string (get_param amp_state (3) "1")) + (Bigint.of_int amp_state.relative_base))
      else Bigint.of_string (get_param amp_state (3) "1") in
    let a = Bigint.of_string (get_param amp_state (1) m1) in
    let b = Bigint.of_string (get_param amp_state (2) m2) in
    let v = if (Bigint.equal a b) then "1" else "0" in
    let () = set_param amp_state.data loc v in
    { amp_state with pc = (Bigint.(+) amp_state.pc (Bigint.of_int 4)); exit_code = 0}
  | "09" -> 
    let loc = Int.of_string (get_param amp_state 1 m1) in
    let new_relative_base = amp_state.relative_base + loc in
    { 
      amp_state with 
      pc = (Bigint.(+) amp_state.pc (Bigint.of_int 2)); 
      exit_code = 0; 
      relative_base = new_relative_base 
    }
  | "99" -> 
    { amp_state with pc = (Bigint.of_int (-1)); exit_code = -1}
  | _ -> raise (Invalid_argument ("Invalid instruction " ^ opcode))

let rec execute (amp_state : Ampstate.amp_state) =
  let new_amp_state = 
    execute_instruction amp_state in

  match new_amp_state.exit_code with
  | 0 ->
    (  match (Bigint.to_string new_amp_state.pc) with
       | "-1" -> new_amp_state
       | x -> execute { 
           new_amp_state with 
           pc = Bigint.of_string x; 
           input_stack = new_amp_state.input_stack; 
           output_stack = new_amp_state.output_stack
         }
    )  
  | _ -> new_amp_state
