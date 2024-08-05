(* TEST *)

(* In this test we check that two different domains will use distinct
   TLS slots -- just like different threads.

   Note that we are intentionally *not* linking the thread runtime, we
   want to ensure that TLS works correctly (just like DLS) when threads
   are not initialized. *)

module TLS = Thread_local_storage

let log fmt () =
  Printf.printf ("[Domain %d] " ^^ fmt ^^ "\n%!")
    (Domain.self () :> int)

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

(* Domain 2 will run last, and it will not call
   the initializer but TLS.set directly. *)
let run_d2 () =
  TLS.set noisy_slot (-1);
  assert (TLS.get noisy_slot = -1);
  assert (TLS.get noisy_slot = -1);
  log "no initialization." ();
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
