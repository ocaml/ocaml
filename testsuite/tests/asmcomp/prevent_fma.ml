(* TEST
  * native
*)

let ( *. ) x y = Sys.opaque_identity (x *. y)
(* Using opaque_identity should prevent use of FMA. *)

let f x = (x *. x -. x *. x)
    (* The expression above can be compiled in two ways:
       1. First evaluating x' = x *. x, then x' -. x'
       The result is obviously zero.
       2. First evaluating x' = x *. x, then x *. x -. x' as a single evaluation
       step, using fused-multiply-add (or rather sub here).
       FMA computes with increased precision because no rounding of the
       intermediate computation happens.
       In this case, the result is not always exactly 0.

       See issue #10323. *)


let () =
  assert (Int64.bits_of_float (f (sqrt 2.0)) = 0L)
