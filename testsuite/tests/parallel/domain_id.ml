(* TEST
* hasunix
include unix
** bytecode
** native
*)

open Domain

let id () = ()


let newdom_id () =
  let d = Domain.spawn id in
  let n = Domain.get_id d in
  join d;
  (n :> int)

let test_domain_reuse () =
  (* checks that domain slots are getting reused quickly,
     by checking that subsequent domain IDs are an arithmetic
     progression (implies that you're getting the same domain
     over and over, but its ID increases by Max_domains.

     this test has to run first, since it makes assumptions
     about domain IDs *)
  let first = newdom_id () in
  let curr = ref (newdom_id ()) in
  let delta = !curr - first in
  assert (delta > 0);
  for i = 1 to 10000 do
    let next = newdom_id () in
    assert (next - !curr = delta);
    curr := next
  done


let test_different_ids () =
  let d1 = Domain.spawn id in
  let d2 = Domain.spawn id in
  assert (get_id d1 <> get_id d2);
  join d1; join d2;
  let d3 = Domain.spawn id in
  assert (get_id d1 <> get_id d3)

let test_interrupt_routing () =
  (* checks that domains do not get each others interrupts,
     even if they reuse the same internal domain *)
  let d1 = Domain.spawn id in
  join d1;
  let r = Atomic.make false in
  let d2 = Domain.spawn (fun () ->
    Sync.(critical_section (fun () ->
      Atomic.set r true; wait (); Atomic.set r false))) in
  while not (Atomic.get r) do () done;
  Sync.notify (get_id d1); (* d2 must not get this interrupt *)
  for i = 1 to 100000 do
    assert (Atomic.get r = true)
  done;
  Sync.notify (get_id d2);
  join d2;
  assert (Atomic.get r = false)

let () =
  test_domain_reuse ();
  test_interrupt_routing ();
  test_different_ids ();
  print_endline "ok"
