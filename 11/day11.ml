open Core

type color = | Black | White
type turn = | Turn_Left | Turn_Right
type facing =  | Up | Down | Left | Right

let map_color color = match color with
  | 0 -> Black
  | 1 -> White
  | _ -> failwith "bad color value"

let map_turn turn = match turn with
  | 0 -> Turn_Left
  | 1 -> Turn_Right
  | _ -> failwith "bad turn value"

let map_facing facing = match facing with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | 3 -> Right
  | _ -> failwith "bad facing value"

let data = In_channel.read_all "11/input.txt"

let get_instructions () = let memory = Hashtbl.create (module Bigint) in 
  let instructions = String.split ~on:',' data in
  let () = List.iteri instructions 
      ~f:(fun idx instr -> Hashtbl.set memory ~key:(Bigint.of_int idx) ~data:instr) in
  memory

let point_to_string (x,y) = Printf.sprintf ("%d,%d") x y
let string_to_point str = let lst = String.split ~on:',' str in
  match lst with
  | a :: b :: [] -> (Int.of_string a, Int.of_string b)
  | _ -> failwith "nope"

let update_panel panels loc color = Hashtbl.set panels ~key:loc ~data:color
let panel_color panels loc = Hashtbl.find_or_add panels loc ~default:(fun () -> Black)

let update_pos pos (facing : facing) (dir : turn) = 
  let (x,y) = string_to_point pos in
  match (facing, dir) with
  | (Up, Turn_Left) -> (point_to_string (x - 1, y), Left)
  | (Up, Turn_Right) -> (point_to_string (x + 1, y), Right)
  | (Down, Turn_Left) -> (point_to_string (x + 1, y), Right)
  | (Down, Turn_Right) -> (point_to_string (x - 1, y), Left)

  | (Left, Turn_Left) -> (point_to_string (x, y - 1), Down)
  | (Left, Turn_Right) -> (point_to_string (x, y + 1), Up)
  | (Right, Turn_Left) -> (point_to_string (x, y + 1), Up)
  | (Right, Turn_Right) -> (point_to_string (x, y - 1), Down)

let process_outputs panels outputs (pos : string) facing = 
  match outputs with
  | color :: turn :: [] -> 
    let () = update_panel panels pos (map_color (Int.of_string color)) in
    update_pos pos facing (map_turn (Int.of_string turn))
  | _ -> failwith "bad output"


let ship_panels : (string, color) Hashtbl.t = Hashtbl.create (module String)

let rec run state pos facing =
  let new_state = Intcode.execute state in
  match new_state.exit_code with
  | -1 -> new_state
  | 1 -> 
    let outputs = new_state.output_stack in
    (* let () = print_endline (List.to_string outputs ~f:ident) in *)
    let (new_pos, new_facing) = process_outputs ship_panels outputs pos facing in
    let color = panel_color ship_panels new_pos in 
    let inp = match color with
      | Black -> "0"
      | White -> "1" in
    run {
      new_state with 
      input_stack = [inp];
      output_stack = [];
    } new_pos new_facing
  | 0 -> run new_state pos facing
  | _ -> failwith ("unknown exit code" ^ (Int.to_string new_state.exit_code))

let finish = run {
    pc = Bigint.of_int 0;
    data = get_instructions ();
    input_stack = ["0"];
    output_stack = [];
    exit_code = 0;
    relative_base = 0;
  } "0,0" Up

let painted_panels = List.length (Hashtbl.keys ship_panels)
