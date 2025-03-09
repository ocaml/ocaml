(* TEST

 ocamlopt_flags = "-g";
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)

(* This is a regression test for a bug that incorrectly instrumented two nested
   try...with expressions, by treating the innermost exception handler as being
   in tail position, and thus inserting a redundant call to [__tsan_func_exit]
   before the call to [g], causing TSan's shadow stack to underflow after a few
   iterations. *)

let g () = raise Exit [@@inline never]

let rec f n =
  try
    try
      if n >= 1000 then () else g ()
    with Exit -> g () (* Innermost handler *)
  with Exit -> f (n+1)

let () = f 1
