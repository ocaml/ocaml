(* TEST *)

(* Tests that an exception in the alloc_minor callback, during a
   combined allocation, causes already-run allocation callbacks to
   be reflected by deallocation callbacks. *)

exception MyExc of string

module MP = Gc.Memprof

(* Similar infrastructure to stop_start_in_callback test *)

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

(* Raise exceptions from allocation callbacks.

   In the native code backend, we have combined allocations. If a
   single allocation callback from a combined allocation raises an
   exception, none of the blocks in that combined allocation are
   actually allocated. However, some allocation callbacks may have
   already been called, before the exception is raised, so statmemprof
   causes their deallocation callbacks also to be called, so that
   allocation and deallocation callbacks can be matched up.

   In the bytecode backend, there are no combined allocations, so
   these special cases don't apply: allocation callbacks called before
   the one which raises an exception reflect actual allocations which
   happened at that time, so statmemprof doesn't have to fake
   corresponding deallocations. *)

let raise_in_alloc () =
  let n_alloc = ref 0 in  (* number of allocations in current profile *)
  let n_prof = ref 0 in   (* number of profiles *)
  let n_exc = ref 0 in    (* number of exceptions handled *)
  let excs = ref AllocSet.empty in

  (* sets of (profile count, allocation count, size), for each operation *)
  let allocs = ref AllocSet.empty in
  let deallocs = ref AllocSet.empty in

  let record s (p, a, sz) = s := AllocSet.add (p,a,sz) (!s) in
  let dealloc_minor minor = (record deallocs minor; ()) in
  let dealloc_major major = (record deallocs major; ()) in

  let alloc_minor (info:MP.allocation) =
      (incr n_alloc;
       let p = !n_prof in
       let a = !n_alloc in
       let sz = info.size + 1 in (* add 1 for header word *)
       record allocs (p,a,sz);
       (* stop profile N after N allocations *)
       if a >= p then
       (record excs (p,a,sz);
        raise (MyExc "from allocation callback"));
       Some (p, a, sz)) in

  let promote minor = Some minor in
  let alloc_major info = (assert false) in   (* We don't expect any *)

  let tracker = { MP.alloc_minor ;
                  dealloc_minor ;
                  promote ;
                  alloc_major ;
                  dealloc_major } in

  let start () = (incr n_prof;
                  n_alloc := 0;
                  ignore (MP.start ~sampling_rate:1.0 tracker)) in

  let arr = ref [] in

  for i = 1 to 10 do
    start ();
    (try
      arr := (f33 42) :: (!arr);
    with
      MyExc s -> (incr n_exc));
    MP.stop();
    Gc.minor();
  done;
  arr := [];
  Gc.full_major();

  let alloc_size =
      AllocSet.fold (fun (p,a,sz) tot -> tot + sz) (!allocs) 0 in
  let alloc_count = AllocSet.cardinal (!allocs) in
  let dealloc_size =
      AllocSet.fold (fun (p,a,sz) tot -> tot + sz) (!deallocs) 0 in
  let dealloc_count = AllocSet.cardinal (!deallocs) in

  (* Every allocation callback is either raised or deallocated *)
  assert (AllocSet.disjoint (!deallocs) (!excs));
  assert (AllocSet.equal (AllocSet.union (!deallocs) (!excs)) (!allocs));

  (* Each call to f33 would allocates 7 blocks of 33 words,
     (sizes 6, 5, 4, 6, 5, 4, 3) plus the 3 words for the cons cell to
     add the result to !arr, making 8 blocks of 36 words.

     So we see this behaviour, as we iterate through the loop:
       i allocs exn words
       1      1   1     6   6
       2      2   1    11   6+5
       3      3   1    15   6+5+4
       4      4   1    21   6+5+4+6
       5      5   1    26   6+5+4+6+5
       6      6   1    30   6+5+4+6+5+4
       7      7   1    33   6+5+4+6+5+4+3
       8      8   1    36   6+5+4+6+5+4+3+3
       9      8   0    36   6+5+4+6+5+4+3+3
      10      8   0    36   6+5+4+6+5+4+3+3

             52   8   250   total

     and of those "allocations" (most of which never actually take
     place with the native code backend), the profile sees
     deallocations for all except 8 (the ones for which the callbacks
     raise exceptions), which add up to 36 words.

   *)

  assert (dealloc_count = 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 8);
  assert (alloc_count = dealloc_count + !n_exc);

  assert (dealloc_size = (0 +
                          6 +
                          6 + 5 +
                          6 + 5 + 4 +
                          6 + 5 + 4 + 6 +
                          6 + 5 + 4 + 6 + 5 +
                          6 + 5 + 4 + 6 + 5 + 4 +
                          6 + 5 + 4 + 6 + 5 + 4 + 3 +
                          6 + 5 + 4 + 6 + 5 + 4 + 3 + 3 +
                          6 + 5 + 4 + 6 + 5 + 4 + 3 + 3));

  assert (alloc_size = dealloc_size +
                       (6 + 5 + 4 + 6 + 5 + 4 + 3 + 3));
  arr


let _ = raise_in_alloc ()
