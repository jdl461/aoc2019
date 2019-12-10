open Core
open Permutations


let list_to_string lst = List.to_string ~f:ident lst

let data = In_channel.read_all "07/input.txt"
(* let data = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" *)

let get_instructions () = Array.append (Array.of_list (String.split ~on:',' data)) (Array.create ~len:0 "0")


let rec execute (amp_state : Ampstate.amp_state) =
  let new_amp_state = 
    Intcode.execute_instruction amp_state in

  match new_amp_state.exit_code with
  | 0 ->
    (  match new_amp_state.pc with
       | -1 -> new_amp_state
       | x -> execute { 
           new_amp_state with 
           pc = x; 
           input_stack = new_amp_state.input_stack; 
           output_stack = new_amp_state.output_stack
         }
    )  
  | _ -> new_amp_state


let part1 () = 
  let run_sequence phases =
    List.fold phases ~init:"0" 
      ~f:(
        fun acc a -> 
          let new_amp_state = 
            execute {
              pc = 0; 
              data = (get_instructions ()); 
              input_stack = [a; acc]; 
              output_stack = []; 
              exit_code = 0
            } in
          List.last_exn new_amp_state.output_stack
      ) in
  let phases = Permutations.permutations ["0";"1";"2";"3";"4"] in
  let signals = List.map phases ~f:run_sequence in
  let output = List.max_elt signals ~compare:(fun a b -> Int.compare (Int.of_string a) (Int.of_string b)) in
  match output with
  | Some signal -> print_endline signal
  | _ -> failwith "No signal"

let part2 () = 
  let setup_amp phase signal = execute {
      pc = 0;
      data = get_instructions (); 
      input_stack = [phase; signal]; 
      output_stack = []; 
      exit_code = 0
    } in


  let setup_phases phases = 
    match phases with
    | a :: b :: c :: d :: e :: [] ->
      let a1 = setup_amp a "0" in
      let a2 = setup_amp b (List.last_exn a1.output_stack) in
      let a3 = setup_amp c (List.last_exn a2.output_stack) in
      let a4 = setup_amp d (List.last_exn a3.output_stack) in
      let a5 = setup_amp e (List.last_exn a4.output_stack) in
      [a1;a2;a3;a4;a5]
    | _ -> failwith "Error" in


  let rec run_phase amps = match amps with
    | [amp1;amp2;amp3;amp4;(amp5 : Ampstate.amp_state)] ->
      if amp1.exit_code = -1 then [amp1;amp2;amp3;amp4;amp5]
      else 
        let amp1 = execute { amp1 with input_stack = [List.last_exn amp5.output_stack]} in
        let amp2 = execute { amp2 with input_stack = [List.last_exn amp1.output_stack]} in
        let amp3 = execute { amp3 with input_stack = [List.last_exn amp2.output_stack]} in
        let amp4 = execute { amp4 with input_stack = [List.last_exn amp3.output_stack]} in
        let amp5 = execute { amp5 with input_stack = [List.last_exn amp4.output_stack]} in
        run_phase [amp1;amp2;amp3;amp4;amp5]
    | _ -> failwith "Error" in

  let p = Permutations.permutations ["9";"8";"7";"6";"5"] in
  let m = List.map p ~f:setup_phases in
  let x = List.map m ~f:run_phase in
  let a = List.map x ~f:(fun x -> let a = List.last_exn x in Int.of_string (List.last_exn a.output_stack)) in

  let max = List.max_elt a ~compare:Int.compare  in

  match max with 
  | Some m -> print_endline (Int.to_string m)
  | None -> print_endline "Error"

let () = part1 ()
let () = part2 ()