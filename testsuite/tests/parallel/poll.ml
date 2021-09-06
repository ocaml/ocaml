(* TEST
* hasunix
include unix
** bytecode
** native
*)

let rec loop () =
  loop ()

let _ =
  ignore (Domain.spawn loop);
  Gc.full_major();
  print_endline "OK"
