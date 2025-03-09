(* TEST *)

(* Tests the effects of stopping and discarding the current profile
   in an allocation callback, particularly in a combined allocation.

   This test is mainly intended to exercise the handling of tracking
   entries inside statmemprof around profile discarding. Testing that
   we count the right number of samples etc is of secondary
   importance. *)

module MP = Gc.Memprof

(* A combined 7-block 33-word allocation *)

let[@inline never] f33 n =
  ((n, n, (n, n, n, (n,n,n,n,n))), (n, n, (n, n, n, (n,n,n,n,n))))

(* Repeatedly stop sampling and discard the profile in an allocation
   callback. If `restart` is `true, start a fresh profile in the same
   callback. Otherwise, start a fresh profile subsequently (not from
   an allocation callback).

   Because the profile is discarded, even deallocation/promotion
   callbacks are not called.

   In the native code backend, we have combined allocations. If a
   single allocation callback from a combined allocation stops
   sampling and discards the profile, no further allocation callbacks
   from that combined allocation are called (and none of the blocks
   are subsequently traced).

   In the bytecode backend, there are no combined allocations, so
   that special case doesn't apply.
 *)

let discard_in_alloc restart =
  let n_prof = ref 0 in (* number of profiles *)
  let n_alloc = ref 0 in (* allocations in current profile *)
  let allocs = ref 0 in  (* number of sampled allocations *)
  let words = ref 0 in (* total size of sampled allocations *)

  let tref = ref MP.null_tracker in
  let pref = ref (MP.start ~sampling_rate:0.0 MP.null_tracker) in
  let _ = MP.stop() in
  let start () = (incr n_prof;
                  n_alloc := 0;
                  pref := (MP.start ~sampling_rate:1.0 !tref)) in
  let stop () = (MP.stop ();
                 MP.discard (!pref)) in

  let alloc_minor (info:MP.allocation) =
      (incr allocs;
       incr n_alloc;
       words := !words + info.size + 1; (* add 1 for header word *)
       (* stop/discard profile N after N allocations *)
       if (!n_alloc) >= (!n_prof) then (stop(); if restart then start());
       Some (!words)) in (* return a tracker value so entry survives *)

 (* We don't expect any other callbacks *)
  let promote minor = (assert false) in
  let dealloc_minor minor = (assert false) in
  let dealloc_major major = (assert false) in
  let alloc_major info = (assert false) in

  let tracker = { MP.alloc_minor ;
                  dealloc_minor ;
                  promote ;
                  alloc_major ;
                  dealloc_major } in

  let res = ref [] in

  tref := tracker;
  start ();
  res := (f33 42) :: (!res);
  if not restart then start ();
  res := (f33 42) :: (!res);
  if not restart then start ();
  res := (f33 42) :: (!res);
  if not restart then start ();
  res := (f33 42) :: (!res);
  if restart then stop();
  Gc.minor();
  res := [];
  Gc.full_major();

  let bytecode = Sys.(backend_type == Bytecode) in

  (* Computations. Each call to f33 allocates 7 blocks of 33 words,
     (sizes 6, 5, 4, 6, 5, 4, 3) plus the 3 words for the cons cell to
     add the result to !res, making 8 blocks of 36 words. We do it 4
     times, so the true total allocation is 32 blocks of 144 words.

     In the bytecode backend, when restarting profiles, we see all these
     allocations.

     In the bytecode backend, without restarting, we see the first
     allocation of the first call to f33, the first 2 of the next call,
     the first 3 of the third call, and the first 4 of the last
     call. That makes 10 allocations, total size 53 words.

     In the native code backend, without restarting, we see the same
     allocations as in the bytecode backend.

     In the native code backend, when restarting, we can also see the
     cons cell allocations, and these account for some of the
     allocations before each profile is stopped. So we see the first
     allocation of the first call to f33, the first cons cell and the
     first allocation of the next f33, the second cons cell and the
     first 2 allocs of the third call, the third cons cell and the first
     3 allocs of the last call, and the fourth cons cell. That makes 11
     allocations, total size 50 words.

     If this were a better test, it would automatically incorporate
     these calculations, rather than hard-wiring them here. But at least
     I've shown my working. *)

  assert (!allocs = (if restart then (if bytecode then 4 * (7 + 1)
                                         else 1 + 2 + 3 + 4 + 1)
                        else (1 + 2 + 3 + 4)));

  assert (!words = (if restart then (if bytecode
                                        then (4 * (6 + 5 + 4 +
                                                     6 + 5 + 4 + 3 + 3))
                                        else (6 + (3 + 6) + (3 + 6 + 5)
                                                + (3 + 6 + 5 + 4) + 3))
                       else (6 + (6 + 5) + (6 + 5 + 4) + (6 + 5 + 4 + 6))));
  res

let _ = discard_in_alloc true
let _ = discard_in_alloc false
