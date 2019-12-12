open Core


type amp_state = {
  pc : Bigint.t;
  data : (Bigint.t, string) Hashtbl.t;
  input_stack : string List.t;
  output_stack : string List.t;
  exit_code : int;
  relative_base : int;
}

let list_to_string lst = List.to_string ~f:ident lst

let data_to_string data = 
  let c = Hashtbl.keys data |> List.sort ~compare:Bigint.compare in
  let m = List.map c ~f:(fun x -> Hashtbl.find_exn data x) in
  List.to_string m ~f:ident

let to_string (amp_state : amp_state) = Printf.sprintf "%s %s %s %d %d" 
    (Bigint.to_string amp_state.pc) (data_to_string amp_state.data) 
    (list_to_string amp_state.output_stack) amp_state.exit_code amp_state.relative_base
