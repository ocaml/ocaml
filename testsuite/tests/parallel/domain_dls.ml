(* TEST
*)

let check_dls () =
  let k1 = Domain.DLS.new_key (fun () -> 10) in
  let k2 = Domain.DLS.new_key (fun () -> 1.0) in
  Domain.DLS.set k1 100;
  Domain.DLS.set k2 200.0;
  let v1 = Domain.DLS.get k1 in
  let v2 = Domain.DLS.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_dls_domain_reuse () =
  let k1 = Domain.DLS.new_key (fun () -> 100) in
  let k2 = Domain.DLS.new_key (fun () -> 200) in
  let domains = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    Domain.DLS.set k1 31415;
    Domain.DLS.set k2 27182;
    assert (Domain.DLS.get k1 = 31415);
    assert (Domain.DLS.get k2 = 27182))) in
  Array.iter Domain.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    assert(Domain.DLS.get k1 = 100);
    assert(Domain.DLS.get k2 = 200))) in
  Array.iter Domain.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_dls)) in
  check_dls ();
  Array.iter Domain.join domains;
  check_dls_domain_reuse ();
  print_endline "ok"
