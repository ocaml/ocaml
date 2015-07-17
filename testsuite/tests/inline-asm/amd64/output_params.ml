external add_int : int -> int ref -> unit = "%asm" "add_int_stub"
       "add	%0, %1	# add_int
	subq	$1, %1" "imr" "+r" "=" "cc"

let b = ref 6
let c = [| b |]
let () =
  (* Test writing to a [Simplif.eliminate_ref] eliminated reference *)
  let a = ref 6 in
  add_int 1 a;
  assert (!a = 7);
  (* Test writing to a non-eliminated reference given as a variable *)
  add_int 1 b;
  assert (!b = 7);
  (* Test writing to a non-eliminated reference not given as a variable *)
  add_int 1 c.(0);
  assert (!(c.(0)) = 8)

(* Like previous but for [float] *)

external add_float : float -> float ref -> unit = "%asm" "add_float_stub"
       "addsd	%0, %1	# add_float" "x" "+x" "=" "cc"

let b = ref 6.
let c = [| b |]
let () =
  let a = ref 6. in
  add_float 1. a;
  assert (!a = 7.);
  add_float 1. b;
  assert (!b = 7.);
  add_float 1. c.(0);
  assert (!(c.(0)) = 8.)

(* Like previous but for [int64] *)

external add_int64 : int64 -> int64 ref -> unit = "%asm" "add_int64_stub"
       "add	%0, %1	# add_int64" "imr" "+r" "=" "cc"

let b = ref 6L
let c = [| b |]
let () =
  let a = ref 6L in
  add_int64 1L a;
  assert (!a = 7L);
  add_int64 1L b;
  assert (!b = 7L);
  add_int64 1L c.(0);
  assert (!(c.(0)) = 8L)
