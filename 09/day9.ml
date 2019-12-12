open Core

let data = In_channel.read_all "09/input.txt"

let get_instructions () = let h = Hashtbl.create (module Bigint) in 
  let instructions = String.split ~on:',' data in
  let () = List.iteri instructions ~f:(fun idx instr -> Hashtbl.set h ~key:(Bigint.of_int idx) ~data:instr) in
  h

let day1 () = 
  let r = Intcode.execute {
      pc = Bigint.of_int 0;
      data = get_instructions ();
      input_stack = ["1"];
      output_stack = [];
      exit_code = 0;
      relative_base = 0;
    } in

  print_endline (Ampstate.list_to_string r.output_stack)

let () = day1 ()