(* TEST *)

let check_dls () =
  let k1 = Thread_local_storage.Key.create (fun () -> 10) in
  let k2 = Thread_local_storage.Key.create (fun () -> 1.0) in
  Thread_local_storage.set k1 100;
  Thread_local_storage.set k2 200.0;
  let v1 = Thread_local_storage.get k1 in
  let v2 = Thread_local_storage.get k2 in
  assert (v1 = 100);
  assert (v2 = 200.0);
  Gc.major ()

let check_dls_domain_reuse () =
  let k1 = Thread_local_storage.Key.create (fun () -> 100) in
  let k2 = Thread_local_storage.Key.create (fun () -> 200) in
  let domains = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    Thread_local_storage.set k1 31415;
    Thread_local_storage.set k2 27182;
    assert (Thread_local_storage.get k1 = 31415);
    assert (Thread_local_storage.get k2 = 27182))) in
  Array.iter Domain.join domains;
  Gc.full_major ();
  let domains2 = Array.init 4 (fun _ -> Domain.spawn(fun _ ->
    assert(Thread_local_storage.get k1 = 100);
    assert(Thread_local_storage.get k2 = 200))) in
  Array.iter Domain.join domains2

let _ =
  let domains = Array.init 3 (fun _ -> Domain.spawn(check_dls)) in
  check_dls ();
  Array.iter Domain.join domains;
  check_dls_domain_reuse ();
  print_endline "ok"
