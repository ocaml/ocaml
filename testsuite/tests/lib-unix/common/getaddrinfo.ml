(* TEST
* hasunix
include unix
** bytecode
** native
*)

let () =
  let x = Unix.getaddrinfo "\000" "" [] in
  Gc.full_major ();
  assert (x = []);;

let () =
  let x = Unix.getaddrinfo "" "\000" [] in
  Gc.full_major ();
  assert (x = []);;
