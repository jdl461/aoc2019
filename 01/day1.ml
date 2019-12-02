open Core

(* part 2 4839845 *)
let rec calc_fuel weight = 
  if weight < 7 then 0
  else 
    let fuel = (weight / 3) - 2 in 
    (calc_fuel fuel) + fuel

let weights = List.map (In_channel.read_lines "input.txt") ~f:Int.of_string
let fuels = List.map weights ~f:calc_fuel
let sum = List.sum (module Int) fuels ~f:ident
let () = print_endline (Int.to_string sum)

