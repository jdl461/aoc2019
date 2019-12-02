open Core

(** part 1 answer 12490719 **)

let input = In_channel.read_all "02/input.txt"
let opcodes = Array.map (Array.of_list (String.split ~on:',' input)) ~f:Int.of_string

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
  | _ -> raise (Invalid_argument "Invalid opcode")

let rec execute pc data =
  let new_pc = execute_instruction pc data in
  match new_pc with
  | -1 -> Array.get data 0
  | x -> execute x data

let v = execute 0 opcodes 
let () = print_endline (Int.to_string v)