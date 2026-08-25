(* TEST
 hasunix;
 include unix;
 ocamlopt_flags += " -O3 ";
 { bytecode; } { native; }
*)

(* This test performs busy-waiting on a thunk being forced by a concurrent domain.
   This is a terrible idea but it should not blow up. *)
let rec safe_force l =
  try Lazy.force l with
  | Lazy.Undefined ->
      Domain.cpu_relax ();
      safe_force l

let mut = Mutex.create ()
let () = Mutex.lock mut

let thunk_calls = Atomic.make 0

(* We use a thunk that will wait until [mut]
   is unlocked to run, and then wait a bit. *)
let l = lazy (
  Mutex.protect mut (fun () -> Unix.sleepf 0.1);
  Atomic.incr thunk_calls
)

(* start forcing the thunk in another domain *)
let d1 = Domain.spawn (fun () -> safe_force l)

(* unlock [mut] to start sleeping *)
let () = Mutex.unlock mut

(* we also force in the main domain,
   which should run into the busy-waiting behavior of [safe_force] *)
let t2 = safe_force l

let () = Domain.join d1

let () =
  (* ensure that the thunk was called exactly once *)
  assert (Atomic.get thunk_calls = 1)
