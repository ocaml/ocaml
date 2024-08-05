(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* In this test we check that two different domains and threads will use distinct
   TLS slots. *)

module TLS = Thread_local_storage

let log fmt () =
  Printf.printf ("[Domain %d, Thread %d] " ^^ fmt ^^ "\n%!")
    (Domain.self () :> int)
    (Thread.id (Thread.self ()))

let noisy =
  let counter = Atomic.make 0 in
  fun () ->
    let number = Atomic.fetch_and_add counter 1 in
    log "initialized value %d." () number;
    number

let noisy_slot = TLS.make noisy

(* Domain 1 will access the slot first,
   it should get id 0. *)
let run_d1 () =
  assert (TLS.get noisy_slot = 0);
  (* accessing the key again does not re-initialize it. *)
  assert (TLS.get noisy_slot = 0);
  ()

(* Domain 2 will run last, and will create two threads
   that will themselves initialize their slot, and
   then initialize its own slot. *)
let run_d2 () =
  let run_thread expected_id =
    assert (TLS.get noisy_slot = expected_id);
    assert (TLS.get noisy_slot = expected_id);
  in
  let t1 = Thread.create run_thread 2 in
  Thread.join t1;
  let t2 = Thread.create run_thread 3 in
  Thread.join t2;
  assert (TLS.get noisy_slot = 4);
  assert (TLS.get noisy_slot = 4);
  ()

(* Domain 0 spawns Domain 1, waits on it, then
   asks for a key itself, it should get id 1.
   Then it runs Domain 2, and checks its own key again. *)
let run_d0 () =
  let d1 = Domain.spawn run_d1 in
  Domain.join d1;
  assert (TLS.get noisy_slot = 1);
  let d2 = Domain.spawn run_d2 in
  Domain.join d2;
  assert (TLS.get noisy_slot = 1);
  ()

let () = run_d0 ()
