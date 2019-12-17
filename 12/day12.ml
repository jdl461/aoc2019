open Core

let ma = (10,15,7)
let mb = (15,10,0)
let mc = (20,12,3)
let md = (0,-3,13)


(* let ma = (-1, 0, 2)
   let mb = (2, -10, -7)
   let mc = (4, -8, 8)
   let md = (3, 5, -1) *)


let moon_to_string (x1,y1,z1) = (Printf.sprintf "<%d,%d,%d>" x1 y1 z1)
let print_moon (m,v) = print_endline @@ (moon_to_string m) ^ " " ^ (moon_to_string v)

let print_step step = 
  match step with
  | [a;b;c;d] -> 
    List.iter [a;b;c;d] ~f:(fun m -> (print_moon m))
  | _ -> failwith "bad output"

let compare (x1,y1,z1) (x2,y2,z2) = 
  (
    (Int.compare x1 x2) * (-1),
    (Int.compare y1 y2) * (-1),
    (Int.compare z1 z2) * (-1)
  )

let update (x1,y1,z1) (x2,y2,z2) = (x1 + x2, y1 + y2, z1 + z2)

let calc_velocity moon_list cur_vel = 
  let mvs = List.map moon_list ~f:(fun (m1,m2) -> compare m1 m2) in
  List.fold mvs ~init:cur_vel ~f:update

let add_abs (x,y,z) = (abs x) + (abs y) + (abs z)
let potential_energy pos = add_abs pos
let kinetic_energy vel = add_abs vel

let pairs m1 m2 m3 m4 =
  [
    (m1,m2);
    (m1,m3);
    (m1,m4)
  ]

let rec step count (m1, v1) (m2, v2) (m3, v3) (m4, v4) = 
  match count with
  | 0 -> [[ (m1, v1); (m2, v2); (m3, v3); (m4, v4) ]]
  | _ -> 
    let m1s = pairs m1 m2 m3 m4 in
    let m1v = calc_velocity m1s v1 in
    let new_m1 = update m1 m1v in

    let m2s = pairs m2 m1 m3 m4 in
    let m2v = calc_velocity m2s v2 in
    let new_m2 = update m2 m2v in

    let m3s = pairs m3 m1 m2 m4 in
    let m3v = calc_velocity m3s v3 in
    let new_m3 = update m3 m3v in

    let m4s = pairs m4 m1 m2 m3 in
    let m4v = calc_velocity m4s v4 in
    let new_m4 = update m4 m4v in
    List.append 
      [[ (new_m1, m1v); (new_m2, m2v); (new_m3, m3v); (new_m4, m4v) ]]
      (step (count - 1) (new_m1, m1v) (new_m2, m2v) (new_m3, m3v) (new_m4, m4v))

let zero_v = (0,0,0)

let steps = step 1000 (ma, zero_v) (mb, zero_v) (mc, zero_v) (md, zero_v)

let last_step = List.last steps

let calc_energy step = match step with
  | [(p1,v1);(p2,v2);(p3,v3);(p4,v4)] -> 
    let pot1 = potential_energy p1 in
    let kin1 = kinetic_energy v1 in
    let pot2 = potential_energy p2 in
    let kin2 = kinetic_energy v2 in
    let pot3 = potential_energy p3 in
    let kin3 = kinetic_energy v3 in
    let pot4 = potential_energy p4 in
    let kin4 = kinetic_energy v4 in
    (pot1 * kin1) + (pot2 * kin2) + (pot3 * kin3) + (pot4 * kin4)
  | _ -> 0

let total_energy = match last_step with
  | Some step -> let () = List.iter step ~f:(
      fun p -> print_moon p
    ) in 
    calc_energy step
  | None -> failwith "bad output"

let () = print_endline (Printf.sprintf "%d" total_energy)
