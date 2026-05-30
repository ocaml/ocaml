(* TEST
 flambda;
 ocamlopt_flags += " -O3 ";
 native;
*)

(* Test that the fix for Issue #14828 didn't degrade inlining.
   In 'result' there is a call that might look recursive but isn't.
   It should still be inlined.

   This is asserted by verifying that the result of calls to 'result' is known.
*)

let f (g, x) = g x
let id x = x
let result n = f (f, (id, n))

let[@inline never][@local never] g () =
  let r = result 42 in
  (* If the result is known, this is statically allocated, otherwise this is
     a runtime allocation.
     g is preventing from being inlined to avoid this being lifted and removing
     the allocation by another mean. *)
  (r, r)

let _ =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let _ = (Sys.opaque_identity g) () in
  let x2 = Gc.allocated_bytes () in
  assert (x1 -. x0 = x2 -. x1)
