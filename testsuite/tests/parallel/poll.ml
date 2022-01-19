(* TEST
* hasunix
include unix
** bytecode
** native
*)

let continue = Atomic.make true

let rec loop () =
  if Atomic.get continue then loop ()

let _ =
  let d = Domain.spawn loop in
  Unix.sleepf 0.1;
  Gc.full_major();
  Atomic.set continue false;
  Domain.join d;
  print_endline "OK"
