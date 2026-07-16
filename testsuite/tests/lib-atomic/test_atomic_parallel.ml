(* TEST *)

(* Stress the atomic read-modify-write primitives (fetch_and_add, exchange,
   compare_and_set) concurrently across domains, and check results that only
   come out right if their acquire/release ordering is preserved.  A broken
   ordering shows up as lost updates or a torn publication. *)

let ndom = max 2 (min 8 (Domain.recommended_domain_count ()))

let spawn_all f =
  let ds = Array.init ndom (fun i -> Domain.spawn (fun () -> f i)) in
  Array.iter Domain.join ds

(* fetch_and_add: every increment must be counted exactly once. *)
let test_fetch_add () =
  let n = 100_000 in
  let c = Atomic.make 0 in
  spawn_all (fun _ -> for _ = 1 to n do ignore (Atomic.fetch_and_add c 1) done);
  assert (Atomic.get c = ndom * n)

(* exchange as a test-and-set spinlock guarding a non-atomic counter:
   the plain [incr] is only safe because the exchange orders the critical
   section; a broken ordering loses updates. *)
let test_exchange_lock () =
  let n = 20_000 in
  let lock = Atomic.make 0 in
  let counter = ref 0 in
  spawn_all (fun _ ->
    for _ = 1 to n do
      while Atomic.exchange lock 1 <> 0 do Domain.cpu_relax () done;
      incr counter;
      ignore (Atomic.exchange lock 0)
    done);
  assert (!counter = ndom * n)

(* Treiber stack via compare_and_set: concurrent pushes then a sequential
   drain.  Both the node count and the value sum must match, which needs each
   pushed node to be published correctly by the CAS. *)
type 'a lst = Nil | Cons of 'a * 'a lst
let test_cas_stack () =
  let per = 20_000 in
  let top = Atomic.make Nil in
  spawn_all (fun d ->
    let v = d + 1 in
    for _ = 1 to per do
      let rec push () =
        let old = Atomic.get top in
        if not (Atomic.compare_and_set top old (Cons (v, old))) then push ()
      in push ()
    done);
  let count = ref 0 and sum = ref 0 in
  let rec drain () = match Atomic.get top with
    | Nil -> ()
    | Cons (v, next) as old ->
      if Atomic.compare_and_set top old next
      then (incr count; sum := !sum + v; drain ()) else drain ()
  in drain ();
  assert (!count = ndom * per);
  assert (!sum = per * (ndom * (ndom + 1) / 2))

(* Publication: the producer fills a multi-word payload (non-atomic) then
   publishes with Atomic.exchange (release); the consumer waits on the flag
   with Atomic.get (acquire) and checks every word.  A broken release/acquire
   lets the consumer see a torn/stale payload. *)
let test_publication () =
  let width = 32 and rounds = 50_000 in
  let payload = Array.make width 0 in
  let flag = Atomic.make 0 and ack = Atomic.make 0 in
  let consumer = Domain.spawn (fun () ->
    for r = 1 to rounds do
      while Atomic.get flag <> r do Domain.cpu_relax () done;
      for i = 0 to width - 1 do assert (Array.unsafe_get payload i = r) done;
      ignore (Atomic.exchange ack r)
    done) in
  for r = 1 to rounds do
    for i = 0 to width - 1 do Array.unsafe_set payload i r done;
    ignore (Atomic.exchange flag r);
    while Atomic.get ack <> r do Domain.cpu_relax () done
  done;
  Domain.join consumer

let () =
  test_fetch_add ();
  test_exchange_lock ();
  test_cas_stack ();
  test_publication ()
