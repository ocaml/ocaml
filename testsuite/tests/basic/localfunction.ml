(* TEST *)

let f x =
  let r = ref 0 in
  let ret x = r := x in
  let[@local] g y = ret (x * y) in
  begin match x with
  | 0 -> ret 0
  | 1 -> g 10
  | _ ->
      if x < 10 then g 20 else g 30
  end;
  !r

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let r = ref 0 in
  for i = 0 to 20 do r := !r + f i done;
  let x2 = Gc.allocated_bytes () in
  Printf.printf "%i\n%!" !r;
  assert(x1 -. x0 = x2 -. x1)
     (* check that we did not allocated anything between x1 and x2 *)


let () =
  (* #8558 *)
  let f () = () in
  let r = ref 0 in
  let g () = f (incr r) in
  g ();
  assert (!r = 1)
