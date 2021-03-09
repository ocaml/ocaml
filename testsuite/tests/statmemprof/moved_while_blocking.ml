(* TEST
* hassysthreads
include systhreads
** bytecode
** native
*)

let t2_begin = Atomic.make false
let t2_promoting = Atomic.make false
let t2_finish_promote = Atomic.make false
let t2_done = Atomic.make false
let t2_quit = Atomic.make false
let await a =
  while not (Atomic.get a) do Thread.yield () done
let set a =
  Atomic.set a true

(* no-alloc printing to stdout *)
let say msg =
  Unix.write Unix.stdout (Bytes.unsafe_of_string msg) 0 (String.length msg) |> ignore

let static_ref = ref 0
let global = ref static_ref
let thread_fn () =
  await t2_begin;
  say "T2: alloc\n";
  let r = ref 0 in
  global := r;
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
  Gc.Memprof.(start ~sampling_rate:1.
    { null_tracker with
      alloc_minor = (fun _ ->
        say "    minor alloc\n";
        Some ());
      alloc_major = (fun _ ->
        say "    major alloc\n";
        Some "major block\n");
      promote = (fun () ->
        say "    promoting...\n";
        set t2_promoting;
        await t2_finish_promote;
        say "    ...done promoting\n";
        Some "promoted block\n");
      dealloc_major = (fun msg ->
        say "    major dealloc: "; say msg) });
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
