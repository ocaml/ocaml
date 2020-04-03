(* TEST *)

module E = Ephemeron.K1

let e = E.create ()

let _ =
  let k = ref (ref 0) in
  E.set_key e !k;
  assert (E.check_key e);
  Gc.full_major ();
  assert (E.check_key e);
  let d = Domain.spawn (fun () ->
    let d = ref 0 in
    E.set_data e d;
    assert (E.check_key e);
    assert (E.check_data e);
    Gc.full_major ();
    assert (E.check_key e);
    assert (E.check_data e))
  in
  Domain.join d;
  assert (E.check_key e);
  assert (E.check_data e);
  Gc.full_major ();
  assert (E.check_key e);
  assert (E.check_data e);
  assert (!(!k) = 0);
  k := ref 0;
  Gc.full_major ();
  assert (not (E.check_key e));
  assert (not (E.check_data e));
  print_endline "ok"
