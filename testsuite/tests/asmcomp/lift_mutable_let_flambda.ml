(* TEST
   * flambda
   ** native
*)

type t = T of { pos : int }

let[@inline always] find_pos i =
  let i = ref i in
  let pos = !i in
  T {pos}

let[@inline always] use_pos i =
  let (T {pos}) = find_pos i in
  pos * 2


let f () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in

  let n : int = (Sys.opaque_identity use_pos) 10 in

  let x2 = Gc.allocated_bytes () in
  assert (n = 20);
  assert(x1 -. x0 = x2 -. x1) (* check no allocation between x1 and x2 *)
  [@@inline never]

let () = f ()
