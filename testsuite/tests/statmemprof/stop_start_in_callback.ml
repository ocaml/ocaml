(* TEST *)

(* Tests the effects of stopping and starting profiles in allocation
   callbacks, particularly in combined allocations.

   This also tests that promotion and deallocation callbacks from old
   profiles get called correctly even after the profile has stopped
   sampling. *)

module MP = Gc.Memprof

(* We need sets of 3-tuples of integers *)

module Int3Tuples =
struct
  type t = int * int * int
  let compare (x0,y0,z0) (x1,y1,z1) =
    match Stdlib.compare x0 x1 with
    | 0 -> (match Stdlib.compare y0 y1 with
            | 0 -> Stdlib.compare z0 z1
            | c -> c)
    | c -> c
end

module AllocSet = Set.Make(Int3Tuples)

(* A combined 7-block 33-word allocation *)

let[@inline never] f33 n =
  ((n, n, (n, n, n, (n,n,n,n,n))), (n, n, (n, n, n, (n,n,n,n,n))))

(* Repeatedly stop sampling from an allocation callback. If `restart`
   is `true, start a fresh profile in the same callback. Otherwise,
   start a fresh profile subsequently (not from an allocation
   callback).

   In the native code backend, we have combined allocations. If a
   single allocation callback from a combined allocation stops
   sampling and starts a new profile, blocks from that combined
   allocation are not subsequently traced.

   However, blocks whose allocation callbacks have already been called
   do have deallocation callbacks also called, so that allocation and
   deallocation callbacks can be matched up.

   If an allocation callback from a combined allocation stops
   sampling, but doesn't start a new profile, the behaviour is much
   simpler: blocks whose allocation callbacks have already been called
   are tracked as usual.

   In the bytecode backend, there are no combined allocations, so
   these special cases don't apply.
 *)

let stop_in_alloc restart =
  let n_alloc = ref 0 in  (* number of allocations in current profile *)
  let n_prof = ref 0 in   (* number of profiles *)

  (* sets of (profile count, allocation count, size), for each operation *)
  let allocs = ref AllocSet.empty in
  let promotes = ref AllocSet.empty in
  let deallocs_minor = ref AllocSet.empty in
  let deallocs_major = ref AllocSet.empty in

  let record s (p, a, sz) = s := AllocSet.add (p,a,sz) (!s) in
  let promote minor = (record promotes minor; Some minor) in
  let dealloc_minor minor = (record deallocs_minor minor; ()) in
  let dealloc_major major = (record deallocs_major major; ()) in

  let tref = ref MP.null_tracker in
  let start () = (incr n_prof;
                  n_alloc := 0;
                  ignore (MP.start ~sampling_rate:1.0 !tref)) in

  let alloc_minor (info:MP.allocation) =
      (incr n_alloc;
       let p = !n_prof in
       let a = !n_alloc in
       let sz = info.size + 1 in (* add 1 for header word *)
       record allocs (p,a,sz);
       (* stop profile N after N allocations *)
       if a >= p then
           (MP.stop ();
            if restart then start())
       else ();
       Some (p, a, sz)) in

  let alloc_major info = (assert false) in (* We don't expect any *)

  let tracker = { MP.alloc_minor ;
                  dealloc_minor ;
                  promote ;
                  alloc_major ;
                  dealloc_major } in
  let arr = ref [] in

  tref := tracker;
  start ();

  arr := (f33 42) :: (!arr);
  if not restart then start ();
  arr := (f33 42) :: (!arr);
  if not restart then start ();
  arr := (f33 42) :: (!arr);
  if not restart then start ();
  arr := (f33 42) :: (!arr);
  if restart then MP.stop();
  Gc.minor();
  arr := [];
  Gc.full_major();

  let alloc_size =
      AllocSet.fold (fun (p,a,sz) tot -> tot + sz) (!allocs) 0 in
  let alloc_count = AllocSet.cardinal (!allocs) in
  let bytecode = Sys.(backend_type == Bytecode) in

  (* Everything promoted is then dealloc'ed from the major heap *)
  assert (AllocSet.subset (!promotes) (!deallocs_major));

  (* Everything deallocated was previously allocated *)
  assert (AllocSet.subset (!deallocs_minor) (!allocs));
  assert (AllocSet.subset (!deallocs_major) (!allocs));

  (* Each block is only deallocated from one heap *)
  assert (AllocSet.disjoint (!deallocs_minor) (!deallocs_major));

  (* Every allocated block is deallocated somewhere *)
  assert (AllocSet.equal (AllocSet.union (!deallocs_minor) (!deallocs_major))
                         (!allocs));

  (* Computations. Each call to f33 allocates 7 blocks of 33 words,
     (sizes 6, 5, 4, 6, 5, 4, 3) plus the 3 words for the cons cell to
     add the result to !arr, making 8 blocks of 36 words. We do it 4
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

  assert (alloc_count = (if restart then (if bytecode then 4 * (7 + 1)
                                             else 1 + 2 + 3 + 4 + 1)
                            else (1 + 2 + 3 + 4)));

  assert (alloc_size = (if restart then (if bytecode
                                         then (4 * (6 + 5 + 4 +
                                                       6 + 5 + 4 + 3 + 3))
                                            else (6 + (3 + 6) + (3 + 6 + 5)
                                                    + (3 + 6 + 5 + 4) + 3))
                           else (6 + (6 + 5) + (6 + 5 + 4) + (6 + 5 + 4 + 6))));
  arr


let _ = stop_in_alloc true
let _ = stop_in_alloc false
