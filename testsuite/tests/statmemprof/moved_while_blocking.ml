(* TEST
 include systhreads;
 hassysthreads;
 {
   bytecode;
 }{
   native;
 }
*)

(* A few triggers, to control timing of events between threads.
   `await a` will wait until after `set a` has been called. *)

let t2_begin = Atomic.make false
let t2_promoting = Atomic.make false
let t2_finish_promote = Atomic.make false
let t2_done = Atomic.make false
let t2_quit = Atomic.make false

(* `await a` waits for the trigger `a` *)
let await a =
  while not (Atomic.get a) do Thread.yield () done

(* `set a` pulls the trigger `a` *)
let set a =
  Atomic.set a true

(* no-alloc printing to stdout *)
let say msg =
  Unix.write_substring Unix.stdout msg 0 (String.length msg)
  |> ignore

(*

The intended sequence of events in this test is as follows:

- thread 1 spawns thread 2 to run thread_fn.

- thread 2 starts thread_fn, waits for t2_begin.

- thread 1 starts a profile, sampling at 100%, which logs allocations and
  has a complex "promote" callback which hands control flow back and
  forth between threads.
- thread 1 allocates a large object (creating tracking entry 0), then
  sets t2_begin and awaits t2_promoting. The alloc_major callback is run at
  some point here, so tracking entry 0 now has no pending callbacks.

- thread 2 wakes on t2_begin.
- thread 2 allocates a small object, a ref cell, on the minor heap. This
  creates tracking entry 1, and runs the alloc_minor callback.
- thread 2 commands a minor collection.
- In the minor collection, the small object is promoted. Tracking entry 1 is
  now marked as promoted and having a runnable callback.
- The promotion callback runs (thread 2 runs this, because thread 1
  is still waiting for t2_promoting). In the promotion callback, t2_promoting
  is set, and then t2_finish_promote is awaited.

- thread 1 wakes on t2_promoting, clears its root, and sets off a full
  major collection which should collect thread 1's large block. The
  after-major-GC function runs, marking tracking entry 0 as deallocated.
- thread 1 then sets t2_finish_promote and awaits t2_done.

- thread 2 wakes on t2_finish_promote, finishes its promotion callback, then
  returns to its main flow of control, clearing the reference to its small
  block, setting t2_done and awaiting t2_quit.

- thread 1 wakes on t2:done, does another full collection, which should
  free the small block from thread 2 and mark its tracking entry for a dealloc
  callback. Then it stops the profile, sets t2_quit, and joins thread 2.

- thread 2 wakes on t2_quit and exits.

- thread 1 joins thread 2 and exits.

Note that the implementation of threads in the bytecode backend
performs some allocations of its own. TODO: update these to use
CAML_DONT_TRACK to avoid statmemprof. For now, I have tweaked the test
so that it doesn't track minor allocations of sizes larger than 1.

*)

let static_ref = ref 0
let global = ref static_ref
let thread_fn () =
  await t2_begin;
  say "T2: alloc\n";
  global := ref 0;
  say "T2: minor GC\n";
  Gc.minor ();
  global := static_ref;
  say "T2: done\n";
  set t2_done;
  await t2_quit

let big = ref [| |]

let fill_big () = big := Array.make 1000 42
  [@@inline never] (* Prevent flambda to move the allocated array in a global
                      root (see #9978). *)
let empty_big () = big := [| |]
  [@@inline never]

let () =
  let th = Thread.create thread_fn () in
  let _:Gc.Memprof.t = Gc.Memprof.(start ~sampling_rate:1.
    { null_tracker with
      alloc_minor = (fun info -> say "    minor alloc\n"; Some ());
      alloc_major = (fun _ -> say "    major alloc\n"; Some "major block\n");
      promote = (fun () ->
        say "    promoting...\n";
        set t2_promoting;
        await t2_finish_promote;
        say "    ...done promoting\n";
        Some "promoted block\n");
      dealloc_major = (fun msg ->
        say "    major dealloc: "; say msg)})
  in
  say "T1: alloc\n";
  fill_big ();
  set t2_begin;
  await t2_promoting;
  say "T1: major GC\n";
  empty_big ();
  Gc.full_major ();
  set t2_finish_promote;
  await t2_done;
  say "T1: major GC\n";
  Gc.full_major ();
  say "T1: done\n";
  Gc.Memprof.stop ();
  set t2_quit;
  Thread.join th
