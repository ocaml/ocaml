(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 : int Domain.DLS.key = Domain.DLS.new_key () in
  let k2 : float Domain.DLS.key = Domain.DLS.new_key () in
  Domain.DLS.set k1 100;
  Domain.DLS.set k2 200.0;
  let v1 = Option.get (Domain.DLS.get k1) in
  let v2 = Option.get (Domain.DLS.get k2) in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check)) in
  check ();
  Array.iter Domain.join domains;
  print_endline "ok"
