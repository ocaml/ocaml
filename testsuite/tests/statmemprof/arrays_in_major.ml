(* TEST *)

module MP = Gc.Memprof

(* Tests that array allocation in the major heap is properly counted
   and managed by statmemprof. *)

(* Allocate arrays of all sizes from `lo` to `hi`, `cnt` times. If
  `keep`, then keep all the arrays, otherwise discard them all. *)

let root = ref []
let[@inline never] allocate_arrays lo hi cnt keep =
  assert (lo >= 300);  (* Will be allocated in major heap. *)
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := Array.make i 0 :: !root
    done;
    if not keep then root := []
  done

(* Check that no allocation callbacks are called if the sampling rate
   is zero. *)

let check_nosample () =
  Printf.printf "check_nosample\n%!";
  let alloc _ =
    Printf.printf "Callback called with sampling_rate = 0\n";
    assert(false) in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:0.
    { MP.null_tracker with alloc_minor = alloc; alloc_major = alloc; }
  in
  allocate_arrays 300 3000 1 false;
  MP.stop ()

let () = check_nosample ()

(* Cross-check counts of allocations, promotions, and deallocations,
   and check that they change appropriately at major collections
   depending on reachability *)

let check_counts_full_major force_promote =
  Printf.printf "check_counts_full_major\n%!";
  let enable = ref true in
  let nalloc_minor = ref 0 in
  let nalloc_major = ref 0 in
  let npromote = ref 0 in
  let ndealloc_minor = ref 0 in
  let ndealloc_major = ref 0 in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:0.01
    {
      alloc_minor = (fun _ ->
        if not !enable then None
        else Some (incr nalloc_minor));
      alloc_major = (fun _ ->
        if not !enable then None
        else Some (incr nalloc_major));
      promote = (fun _ -> Some (incr npromote));
      dealloc_minor = (fun _ -> incr ndealloc_minor);
      dealloc_major = (fun _ -> incr ndealloc_major);
    }
  in
  allocate_arrays 300 3000 1 true;
  enable := false; (* stop sampling *)
  (* everything is still reachable from root *)
  assert (!ndealloc_minor = 0 && !ndealloc_major = 0);

  if force_promote then begin
    Gc.full_major ();
    (* everything is still reachable from root, and
       everything allocated in the minor heap has now
       been promoted *)
    assert (!ndealloc_minor = 0 && !ndealloc_major = 0 &&
            !npromote = !nalloc_minor);

    root := [];
    Gc.full_major ();
    (* nothing is reachable from root, so everything (which was
       promoted) has now been deallocated in the major heap *)
    assert (!ndealloc_minor = 0 &&
            !ndealloc_major = !nalloc_minor + !nalloc_major);

  end else begin
    root := [];
    Gc.minor ();
    Gc.full_major ();
    Gc.full_major ();
    (* everything allocated in the minor heap has either
       been deallocated in the minor heap or promoted,
       and everything deallocated in the major heap had
       either been allocated in the major heap or promoted *)
    assert (!nalloc_minor = !ndealloc_minor + !npromote &&
            !ndealloc_major = !npromote + !nalloc_major)
  end;
  MP.stop ()

let () =
  check_counts_full_major false;
  check_counts_full_major true

let check_no_nested () =
  Printf.printf "check_no_nested\n%!";
  let in_callback = ref false in
  let cb _ =
    assert (not !in_callback);
    in_callback := true;
    allocate_arrays 300 300 100 false;
    in_callback := false;
    () in
  let cb' _ = cb (); Some () in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:1.
    {
      alloc_minor = cb';
      alloc_major = cb';
      promote = cb';
      dealloc_minor = cb;
      dealloc_major = cb;
    }
  in
  allocate_arrays 300 300 100 false;
  MP.stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  let smp = ref 0 in
  let _:MP.t = MP.start ~callstack_size:10 ~sampling_rate:rate
    { MP.null_tracker with
      alloc_major = (fun info ->
        assert (info.size >= lo && info.size <= hi);
        assert (info.n_samples > 0);
        assert (info.source = Normal);
        smp := !smp + info.n_samples;
        None
      );
    }
  in
  allocate_arrays lo hi cnt false;
  MP.stop ();

  (* The probability distribution of the number of samples follows a
     binomial distribution of parameters tot_alloc and rate. Given
     that tot_alloc*rate and tot_alloc*(1-rate) are large (i.e., >
     100), this distribution is approximately equal to a normal
     distribution. We compute a 1e-8 confidence interval for !smp
     using quantiles of the normal distribution, and check that we are
     in this confidence interval. *)
  let tot_alloc = cnt*(lo+hi+2)*(hi-lo+1)/2 in
  assert (float tot_alloc *. rate > 100. &&
          float tot_alloc *. (1. -. rate) > 100.);
  let mean = float tot_alloc *. rate in
  let stddev = sqrt (float tot_alloc *. rate *. (1. -. rate)) in
  (* This should fail approximately one time in 100,000,000 *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 300 3000 3 0.00001;
  check_distrib 300 3000 1 0.0001;
  check_distrib 300 3000 1 0.01;
  check_distrib 300 3000 1 0.9;
  check_distrib 300 300 100000 0.1;
  check_distrib 300000 300000 30 0.1

let () =
  Printf.printf "OK !\n"
