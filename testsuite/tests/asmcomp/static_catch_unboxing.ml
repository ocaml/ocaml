(* TEST
 native;
*)

let[@inline never][@local never] f b x =
  let[@local] g y =
    y +. x > 0.
  in
  if b then g x else g (x +. 1.)

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  ignore (f true 1.);
  ignore (f false 1.);
  let x2 = Gc.allocated_bytes () in
  assert(x1 -. x0 = x2 -. x1)
