open Core

let data = In_channel.read_all "13/input.txt"

let get_instructions () = let memory = Hashtbl.create (module Bigint) in 
  let instructions = String.split ~on:',' data in
  let () = List.iteri instructions 
      ~f:(fun idx instr -> Hashtbl.set memory ~key:(Bigint.of_int idx) ~data:instr) in
  memory

let x = Intcode.execute {
    pc = Bigint.of_int 0;
    data = get_instructions ();
    input_stack = [];
    output_stack = [];
    exit_code = 0;
    relative_base = 0;
  }

let pixels = List.chunks_of x.output_stack ~length:3


let block_tiles = List.filter pixels ~f:(
    fun p -> 
      match p with
      | [_;_;"2"] -> true
      | _ -> false
  )

let () = print_endline (Int.to_string (List.length block_tiles))

