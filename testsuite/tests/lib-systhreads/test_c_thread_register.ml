(* TEST
 modules = "test_c_thread_register_cstubs.c";
 include systhreads;
 no-tsan; (* Flaky under TSan, disable until fixed (see issue #13472) *)
 hassysthreads;
 not-bsd;
 {
   bytecode;
 }{
   native;
 }
*)

(* spins a external thread from C and register it to the OCaml runtime *)

external spawn_thread : (unit -> unit) -> unit = "spawn_thread"

let passed () = Printf.printf "passed\n"

let _ =
  spawn_thread (passed);
  Thread.delay 0.5
