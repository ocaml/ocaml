let test v =
  let open Atomic in
  assert (get v = 42);
  set v 10;
  assert (get v = 10);
  let b = compare_and_set v 11 20 in
  assert (b = false);
  assert (get v = 10);
  let b = compare_and_set v 10 20 in
  assert (b = true);
  assert (get v = 20)

let () =
  let r = Atomic.make 42 in
  test r;
  Atomic.set r 42;
  Gc.full_major ();
  test r;
  print_endline "ok"
