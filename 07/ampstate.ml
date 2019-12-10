open Core


type amp_state = {
  pc : int;
  data : string Array.t;
  input_stack : string List.t;
  output_stack : string List.t;
  exit_code : int;
}

let list_to_string lst = List.to_string ~f:ident lst

let to_string (amp_state : amp_state) = Printf.sprintf "%d %s %s %d" 
    amp_state.pc (list_to_string @@ Array.to_list amp_state.data) 
    (list_to_string amp_state.output_stack) amp_state.exit_code
