(* TEST *)

module E = Ephemeron.K1

module S = struct
  let make () = Atomic.make false

  let rec wait s =
    let success =
      Domain.Sync.critical_section (fun () ->
        if Atomic.get s then true
        else (Domain.Sync.wait (); false))
    in
    if success then () else wait s

  let notify s d =
    Atomic.set s true;
    Domain.Sync.notify d
end

let e1 = E.create ()

let _ =
  let k = ref (ref 0) in
  E.set_key e1 !k;
  assert (E.check_key e1);
  assert (not (E.check_data e1));
  Gc.full_major ();
  assert (E.check_key e1);
  assert (not (E.check_data e1));

  let s1,s2 = S.make(), S.make() in
  let main_domain = Domain.self () in
  let child_domain = Domain.spawn (fun () ->
    let e2 = E.create () in
    E.set_key e2 !k;
    E.set_data e1 e2;
    assert (E.check_key e1);
    assert (E.check_data e1);
    assert (E.check_key e2);
    assert (not (E.check_data e2));
    Gc.full_major ();
    assert (E.check_key e1);
    assert (E.check_data e1);
    assert (E.check_key e2);
    assert (not (E.check_data e2));
    S.notify s1 main_domain;

    S.wait s2;
    let e3 = match E.get_data e2 with
    | Some e -> e
    | None -> assert false
    in

    Gc.full_major();
    assert (E.check_key e1);
    assert (E.check_data e1);
    assert (E.check_key e2);
    assert (E.check_data e2);
    assert (E.check_key e3);
    assert (E.check_data e3)
  )
  in

  let _ = S.wait s1 in
  let e2 = match E.get_data e1 with
  | Some e -> e
  | None -> assert (false)
  in

  let e3 = E.create () in
  E.set_key e3 !k;
  E.set_data e3 0;
  E.set_data e2 e3;

  Gc.full_major ();
  assert (E.check_key e1);
  assert (E.check_data e1);
  assert (E.check_key e2);
  assert (E.check_data e2);
  assert (E.check_key e3);
  assert (E.check_data e3);

  S.notify s2 (Domain.get_id child_domain);
  Domain.join child_domain;

  Gc.full_major ();
  assert (E.check_key e1);
  assert (E.check_data e1);
  assert (E.check_key e2);
  assert (E.check_data e2);
  assert (E.check_key e3);
  assert (E.check_data e3);

  assert (!(!k) = 0);
  k := ref 0;
  Gc.full_major ();
  assert (not (E.check_key e1));
  assert (not (E.check_data e1));
  assert (not (E.check_key e2));
  assert (not (E.check_data e2));
  assert (not (E.check_key e3));
  assert (not (E.check_data e3));
  print_endline "ok"

