(* TEST
* hasunix
include unix
** bytecode
** native
*)

let _ =
  at_exit (fun () -> print_string "B\n"; flush stdout);
  print_string "A\n"; (* don't flush *)
  Unix._exit 0
