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
  start {
      sampling_rate = 0.;
      callstack_size = 10;
      callback = fun _ ->
        Printf.printf "Callback called with sampling_rate = 0\n";
        assert(false)
  };
  allocate_arrays 300 3000 1 false

let () = check_nosample ()

let check_ephe_full_major () =
  Printf.printf "check_ephe_full_major\n%!";
  let ephes = ref [] in
  start {
    sampling_rate = 0.01;
    callstack_size = 10;
    callback = fun _ ->
      let res = Ephemeron.K1.create () in
      ephes := res :: !ephes;
      Some res
  };
  allocate_arrays 300 3000 1 true;
  stop ();
  List.iter (fun e -> assert (Ephemeron.K1.check_key e)) !ephes;
  Gc.full_major ();
  List.iter (fun e -> assert (Ephemeron.K1.check_key e)) !ephes;
  root := [];
  Gc.full_major ();
  List.iter (fun e -> assert (not (Ephemeron.K1.check_key e))) !ephes

let () = check_ephe_full_major ()

let check_no_nested () =
  Printf.printf "check_no_nested\n%!";
  let in_callback = ref false in
  start {
      sampling_rate = 1.;
      callstack_size = 10;
      callback = fun _ ->
        assert (not !in_callback);
        in_callback := true;
        allocate_arrays 300 300 100 false;
        in_callback := false;
        None
  };
  allocate_arrays 300 300 100 false;
  stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  let smp = ref 0 in
  start {
      sampling_rate = rate;
      callstack_size = 10;
      callback = fun info ->
        (* We also allocate the list constructor in the minor heap. *)
        if info.kind = Major then begin
          assert (info.tag = 0);
          assert (info.size >= lo && info.size <= hi);
          assert (info.n_samples > 0);
          smp := !smp + info.n_samples
        end;
        None
    };
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
  start {
      sampling_rate = 1.;
      callstack_size = 10;
      callback = fun info ->
        if info.kind = Major then callstack := Some info.callstack;
        None
    };
  allocate_arrays 300 300 100 false;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () = check_callstack ()

let () =
  Printf.printf "OK !\n"
