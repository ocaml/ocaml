(* TEST
 modules = "outgoing_stack_args_.c";
*)

(* Regression test for outgoing stack arguments.

   A call whose arguments do not all fit in registers passes the surplus in
   an outgoing-argument area that the caller allocates below its own frame.
   If the caller reserves the wrong amount, the arguments it writes there
   overflow into its own frame.  On POWER, [Istackoffset] must reserve the
   ELFv2 linkage area in addition to the arguments themselves, since
   [slot_offset] places outgoing arguments above it.

   External calls are what reach this path in practice: they follow the C
   ABI, whose register budget is small and has no escape hatch.  (An
   OCaml-to-OCaml call is not enough: arguments that do not fit in registers
   go into domain-state slots first -- 64 of them, see
   [Proc.size_domainstate_args] -- so such a call would need more than 80
   arguments before it used the stack at all.)

   Each call below is made with values held live across it, so an
   overflowing outgoing-argument area shows up as a wrong result. *)

external stack_args_ints :
  int -> int -> int -> int -> int -> int ->
  int -> int -> int -> int -> int -> int -> int
  = "stack_args_ints_byte" "stack_args_ints"

external stack_args_floats :
  (float [@unboxed]) -> (float [@unboxed]) -> (float [@unboxed]) ->
  (float [@unboxed]) -> (float [@unboxed]) -> (float [@unboxed]) ->
  (float [@unboxed]) -> (float [@unboxed]) -> (float [@unboxed]) ->
  (float [@unboxed]) -> (float [@unboxed]) -> (float [@unboxed]) ->
  (float [@unboxed]) -> (float [@unboxed]) -> (float [@unboxed]) ->
  (float [@unboxed]) -> (float [@unboxed])
  = "stack_args_floats_byte" "stack_args_floats"

let () =
  let a = Sys.opaque_identity 1000 in
  let b = Sys.opaque_identity 2000 in
  let c = Sys.opaque_identity 3000 in
  let d = Sys.opaque_identity 4000 in
  let ints = stack_args_ints 1 2 3 4 5 6 7 8 9 10 11 12 in
  let floats =
    stack_args_floats 1. 2. 3. 4. 5. 6. 7. 8. 9. 10. 11. 12. 13. 14. 15. 16.
  in
  (* Arguments taken from live locals, and a result kept live across a
     further call. *)
  let ints2 = stack_args_ints a b c d 1 2 3 4 5 6 7 8 in
  Printf.printf "ints=%d floats=%.1f ints2=%d a=%d b=%d c=%d d=%d\n"
    ints floats ints2 a b c d
