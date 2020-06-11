(* TEST
* hasunix
include unix
** bytecode
** native
*)

let _ =
  let _d = Domain.spawn (fun _ ->
    Unix.sleep 10;
    print_endline "Should not reach here!") in
  Gc.full_major ();
  print_endline "OK"
