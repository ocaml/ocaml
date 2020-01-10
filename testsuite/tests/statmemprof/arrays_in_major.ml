(* TEST
   flags = "-g"
   compare_programs = "false"
*)

open Gc.Memprof

let root = ref []
let[@inline never] allocate_arrays lo hi cnt keep =
  assert (lo >= 300);  (* Will be allocated in major heap. *)
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := Array.make i 0 :: !root
    done;
    if not keep then root := []
  done

let check_nosample () =
  Printf.printf "check_nosample\n%!";
  let cb _ =
    Printf.printf "Callback called with sampling_rate = 0\n";
    assert(false)
  in
  start ~callstack_size:10 ~minor_alloc_callback:cb ~major_alloc_callback:cb
        ~sampling_rate:0. ();
  allocate_arrays 300 3000 1 false

let () = check_nosample ()

let check_counts_full_major force_promote =
  Printf.printf "check_counts_full_major\n%!";
  let nalloc_minor = ref 0 in
  let nalloc_major = ref 0 in
  let enable = ref true in
  let npromote = ref 0 in
  let ndealloc_minor = ref 0 in
  let ndealloc_major = ref 0 in
  start ~callstack_size:10
        ~minor_alloc_callback:(fun _ ->
          if !enable then begin
              incr nalloc_minor;
              Some ()
          end else
            None)
        ~major_alloc_callback:(fun _ ->
           if !enable then begin
             incr nalloc_major;
             Some ()
           end else
             None)
        ~promote_callback:(fun _ ->
           incr npromote;
           Some ())
        ~minor_dealloc_callback:(fun _ -> incr ndealloc_minor)
        ~major_dealloc_callback:(fun _ -> incr ndealloc_major)
        ~sampling_rate:0.01 ();
  allocate_arrays 300 3000 1 true;
  enable := false;
  assert (!ndealloc_minor = 0 && !ndealloc_major = 0);
  if force_promote then begin
    Gc.full_major ();
    assert (!ndealloc_minor = 0 && !ndealloc_major = 0 &&
            !npromote = !nalloc_minor);
    root := [];
    Gc.full_major ();
    assert (!ndealloc_minor = 0 &&
            !ndealloc_major = !nalloc_minor + !nalloc_major);
  end else begin
    root := [];
    Gc.minor ();
    Gc.full_major ();
    Gc.full_major ();
    assert (!nalloc_minor = !ndealloc_minor + !npromote &&
            !ndealloc_major = !npromote + !nalloc_major)
  end;
  stop ()

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
    ()
  in
  let cb' _ = cb (); Some () in
  start ~callstack_size:10
        ~minor_alloc_callback:cb' ~major_alloc_callback:cb'
        ~promote_callback:cb' ~minor_dealloc_callback:cb
        ~major_dealloc_callback:cb
        ~sampling_rate:1. ();
  allocate_arrays 300 300 100 false;
  stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  let smp = ref 0 in
  start ~callstack_size:10
        ~major_alloc_callback:(fun info ->
           assert (info.size >= lo && info.size <= hi);
           assert (info.n_samples > 0);
           assert (not info.unmarshalled);
           smp := !smp + info.n_samples;
           None
        )
        ~sampling_rate:rate ();
  allocate_arrays lo hi cnt false;
  stop ();

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
  (* This assertion has probability to fail close to 1e-8. *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 300 3000 3 0.00001;
  check_distrib 300 3000 1 0.0001;
  check_distrib 300 3000 1 0.01;
  check_distrib 300 3000 1 0.9;
  check_distrib 300 300 100000 0.1;
  check_distrib 300000 300000 30 0.1

let[@inline never] check_callstack () =
  Printf.printf "check_callstack\n%!";
  let callstack = ref None in
  start ~callstack_size:10
        ~major_alloc_callback:(fun info ->
           callstack := Some info.callstack;
           None
        )
        ~sampling_rate:1. ();
  allocate_arrays 300 300 100 false;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () = check_callstack ()

let () =
  Printf.printf "OK !\n"
