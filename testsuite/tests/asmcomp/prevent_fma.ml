(* TEST
  * native
*)

let k_split = 134217729.0

(* This file implements the "double double" inversion algorithm which relies on
   floating point having exactly 64 bits of precision. Inserting
   fused-multiply-add would exceed the precision and produce an incorrect
   result.
   See issue #10323. *)

let ( *. ) x y = Sys.opaque_identity (x *. y)
(** Using opaque_identity should prevent use of FMA,
    that's the main thing tested by this file. *)

type t = {hi: float; lo: float}

let inv {hi; lo} =
  (*C = 1.0/hi; *)
  let c' = 1. /. hi in
  (*c = SPLIT*C; *)
  let c  = k_split *. c' in
  (*hc =c-C;  *)
  let hc = c -. c' in
  (*u = SPLIT*hi;*)
  let u  = k_split *. hi in
  (*hc = c-hc; tc = C-hc; hy = u-hi; U = C*hi; hy = u-hy; ty = hi-hy;*)
  let hc = c  -. hc in
  let tc = c' -. hc in
  let hy = u  -. hi in
  let u' = c' *. hi in
  let hy = u  -. hy in
  let ty = hi -. hy in
  (*u = (((hc*hy-U)+hc*ty)+tc*hy)+tc*ty;*)
  let u  = (((hc *. hy -. u') +. hc *. ty) +. tc *. hy) +. tc *. ty in
  (*c = ((((1.0-U)-u))-C*lo)/hi;*)
  let c  = (((1.0 -. u') -. u) -. c' *. lo) /. hi in
  let hi = c' +. c in
  let lo = (c' -. hi) +. c in
  {hi; lo}

let () =
  (*let a = {hi = 0.34; lo = 0.0} in
  let a' = inv a in*)
  let a' = {
    hi = 2.9411764705882350590115947852609679102898;
    lo = 0.0000000000000000238179334002628905858757;
  } in
  let a'' = inv a' in
  assert (Int64.bits_of_float a''.lo = 0L)
