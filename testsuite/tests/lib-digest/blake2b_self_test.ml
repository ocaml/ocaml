(* TEST
 modules = "blake2b_self_test_stubs.c";
*)

external self_test_main : unit -> unit = "self_test_main"

let () = self_test_main ()
