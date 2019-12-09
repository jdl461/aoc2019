open Core
open Permutations

let data = In_channel.read_all "07/input.txt"
let get_instructions = Array.append (Array.of_list (String.split ~on:',' data)) (Array.create ~len:5000 "0")

let rec execute pc data input_stack output_stack =
  let (new_pc, data, new_input_stack, new_output_stack, exit_code) = Intcode.execute_instruction pc data input_stack output_stack in
  match new_pc with
  | -1 -> new_output_stack
  | x -> execute x data new_input_stack new_output_stack


let run_sequence phases =
  List.fold phases ~init:"0" 
    ~f:(
      fun acc a -> let output = execute 0 get_instructions [a; acc] [] in
        List.last_exn output
    )

let part1 = let phases = Permutations.permutations ["0";"1";"2";"3";"4"] in
  let signals = List.map phases ~f:run_sequence in
  let output = List.max_elt signals ~compare:(fun a b -> Int.compare (Int.of_string a) (Int.of_string b)) in
  match output with
  | Some signal -> print_endline signal
  | _ -> failwith "No signal"
