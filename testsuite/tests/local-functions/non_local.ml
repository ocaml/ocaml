(* TEST *)

(* Basic case: local optimisation works *)
let local_direct x =
  let[@local] f y = x + y in
  f 0

(* Multiple calls in the same tail context work *)
let local_multiple x b =
  let[@local] f y = x + y in
  if b then f 0 else f 1

(* Calls in an inner scope work *)
let local_inner_scope x =
  let[@local] f y = x + y in
  (f 0, 1)

(* Calls inside another function should not be optimised,
   and produce a warning *)
let local_in_function x =
  let[@local] f y = x + y in
  List.map (fun x -> f (succ x)) [0]
