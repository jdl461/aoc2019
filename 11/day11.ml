open Core

type color = | Black | White
type turn = | Turn_Left | Turn_Right
type facing =  | Up | Down | Left | Right

let int_to_color color = match color with
  | 0 -> Black
  | 1 -> White
  | _ -> failwith "bad color value"

let int_to_turn turn = match turn with
  | 0 -> Turn_Left
  | 1 -> Turn_Right
  | _ -> failwith "bad turn value"

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
    let color = Int.of_string color in
    let turn = Int.of_string turn in
    let () = update_panel panels pos (int_to_color color) in
    update_pos pos facing (int_to_turn turn)
  | _ -> failwith "bad output"


let rec run ship_panels state pos facing =
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
    run ship_panels {
      new_state with 
      input_stack = [inp];
      output_stack = [];
    } new_pos new_facing
  | 0 -> run ship_panels new_state pos facing
  | _ -> failwith ("unknown exit code" ^ (Int.to_string new_state.exit_code))


let part1 () =
  let ship_panels : (string, color) Hashtbl.t = Hashtbl.create (module String) in
  let _ : Ampstate.amp_state = run ship_panels {
      pc = Bigint.of_int 0;
      data = get_instructions ();
      input_stack = ["0"];
      output_stack = [];
      exit_code = 0;
      relative_base = 0;
    } "0,0" Up in
  let painted_panels = List.length (Hashtbl.keys ship_panels) in
  print_endline (Printf.sprintf "%d" painted_panels)

let part2 () = 
  let ship_panels : (string, color) Hashtbl.t = Hashtbl.create (module String) in

  let _ : Ampstate.amp_state = run ship_panels {
      pc = Bigint.of_int 0;
      data = get_instructions ();
      input_stack = ["1"];
      output_stack = [];
      exit_code = 0;
      relative_base = 0;
    } "0,0" Up in

  let white_panels = Hashtbl.filter ship_panels 
      ~f:(
        fun color -> match color with 
          | Black -> false
          | White -> true
      ) in


  let lst = Hashtbl.keys white_panels |> List.map ~f:(
      fun pos -> "(" ^ pos ^ ")"
    ) in

  let () = List.iter lst ~f:(fun p -> print_endline p) in

  print_endline (Int.to_string (Hashtbl.length white_panels))
