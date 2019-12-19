open Core

let data = In_channel.read_all "13/input.txt"

let get_instructions () = let memory = Hashtbl.create (module Bigint) in 
  let instructions = String.split ~on:',' data in
  let () = List.iteri instructions 
      ~f:(fun idx instr -> Hashtbl.set memory ~key:(Bigint.of_int idx) ~data:instr) in
  memory

let part1 () = 
  let state = Intcode.execute {
      pc = Bigint.of_int 0;
      data = get_instructions ();
      input_stack = [];
      output_stack = [];
      exit_code = 0;
      relative_base = 0;
    } in
  let pixels = List.chunks_of state.output_stack ~length:3 in
  let block_tiles = List.filter pixels ~f:(
      fun p -> 
        match p with
        | [_;_;"2"] -> true
        | _ -> false
    ) in
  print_endline (Int.to_string (List.length block_tiles))

let update_ball pos bx =
  match pos with
  | [x;_;"4"] -> Int.of_string x
  | _ -> bx

let update_paddle pos px =
  match pos with
  | [x;_;"3"] -> Int.of_string x
  | _ -> px

let get_score output = 
  match output with
  | ["-1"; "0"; score] -> Some score
  | _ -> None

let rec execute (amp_state : Ampstate.amp_state) bx px =
  let new_amp_state = Intcode.execute_instruction amp_state in
  match new_amp_state.exit_code with
  | 0 ->
    execute new_amp_state bx px
  | 1 -> 
    let input = if bx > px then 1 else if bx < px then -1 else 0 in
    let new_input = [Int.to_string input] in
    execute { 
      new_amp_state with 
      input_stack = new_input;
    } bx px
  | 2 -> let output = new_amp_state.output_stack in
    if List.length output < 3
    then
      execute new_amp_state bx px
    else 
      let () = print_endline (List.to_string output ~f:ident) in
      let new_bx = update_ball output bx in
      let new_px = update_paddle output px in
      let score = get_score output in
      let () = match score with
        | Some score -> print_endline ("score: " ^ score)
        | None -> () in
      execute { new_amp_state with output_stack = [] } new_bx new_px
  | _ -> new_amp_state


let x = execute {
    pc = Bigint.of_int 0;
    data = get_instructions ();
    input_stack = [];
    output_stack = [];
    exit_code = 0;
    relative_base = 0;
  } (-1) (-1)
