(* TEST *)

(* Regression test for outgoing stack arguments.

   On targets that pass the first few arguments in registers (e.g. PowerPC
   ELFv2: 8 integer / 13 float argument registers), a call with more
   arguments spills the surplus to an outgoing-argument area on the stack.
   The caller must reserve that area *in addition to* the ABI's fixed
   bottom-of-frame linkage area, otherwise the outgoing arguments overflow
   into the caller's own frame.

   This exercises calls that force outgoing integer stack slots and checks
   that the results are correct and that live locals are preserved across
   the calls.  On PowerPC the overflow is latent for ordinary execution
   (the clobbered slots are the caller's linkage area, which non-frame-
   pointer code neither reads nor relies on); the frame-pointers test suite
   exercises the observable path. *)

let[@inline never] sum10 a b c d e f g h i j =
  a + b + c + d + e + f + g + h + i + j

let[@inline never] sum16 a b c d e f g h i j k l m n o p =
  a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p

let[@inline never] mixed a b c d e f g h i (x : float) =
  float_of_int (a + b + c + d + e + f + g + h + i) +. x

(* Recursion through a many-argument function, so successive frames each
   carry an outgoing-argument region. *)
let[@inline never] rec countdown n acc a b c d e f g h i j =
  if n = 0 then acc + a + b + c + d + e + f + g + h + i + j
  else countdown (n - 1) (acc + 1) a b c d e f g h i j

let () =
  let x = Sys.opaque_identity 1000 in
  let y = Sys.opaque_identity 2000 in
  let s10 = sum10 1 2 3 4 5 6 7 8 9 10 in
  let s16 = sum16 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 in
  let m = mixed 1 2 3 4 5 6 7 8 9 0.5 in
  let c = countdown 100 0 1 2 3 4 5 6 7 8 9 10 in
  (* Live locals must survive the stack-argument calls. *)
  assert (x = 1000);
  assert (y = 2000);
  Printf.printf "s10=%d s16=%d mixed=%.1f countdown=%d x=%d y=%d\n"
    s10 s16 m c x y
