(* TEST
 modules = "stub.c";
*)

external run_tests : unit -> unit = "run_tests"

let () = run_tests ()
