(* TEST
* hasunix
include unix
** bytecode
** native
*)

let check () =
  let k1 = Domain.DLS.new_key (fun () -> 10) in
  let k2 = Domain.DLS.new_key (fun () -> 1.0) in
  Domain.DLS.set k1 100;
  Domain.DLS.set k2 200.0;
  let v1 = Domain.DLS.get k1 in
  let v2 = Domain.DLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check)) in
  check ();
  Array.iter Domain.join domains;
  print_endline "ok"
