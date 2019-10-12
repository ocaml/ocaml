(* TEST
   flags = "-g"
   * bytecode
*)

open Gc.Memprof

let[@inline never] allocate_lists len cnt =
  let rec allocate_list accu = function
    | 0 -> accu
    | n -> allocate_list (n::accu) (n-1)
  in
  for j = 0 to cnt-1 do
    ignore (allocate_list [] len)
  done

let check_distrib len cnt rate =
  Printf.printf "check_distrib %d %d %f\n%!" len cnt rate;
  let smp = ref 0 in
  start {
      sampling_rate = rate;
      callstack_size = 10;
      callback = fun info ->
        assert (info.kind = Minor);
        if info.tag = 0 then begin (* Exclude noise such as spurious closures. *)
          assert (info.size = 2);
          assert (info.n_samples > 0);
          smp := !smp + info.n_samples
        end;
        None
    };
  allocate_lists len cnt;
  stop ();

  (* The probability distribution of the number of samples follows a
     binomial distribution of parameters tot_alloc and rate. Given
     that tot_alloc*rate and tot_alloc*(1-rate) are large (i.e., >
     100), this distribution is approximately equal to a normal
     distribution. We compute a 1e-8 confidence interval for !smp
     using quantiles of the normal distribution, and check that we are
     in this confidence interval. *)
  let tot_alloc = cnt*len*3 in
  assert (float tot_alloc *. rate > 100. &&
          float tot_alloc *. (1. -. rate) > 100.);
  let mean = float tot_alloc *. rate in
  let stddev = sqrt (float tot_alloc *. rate *. (1. -. rate)) in
  (* This assertion has probability to fail close to 1e-8. *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 10 1000000 0.01;
  check_distrib 1000000 10 0.00001;
  check_distrib 1000000 10 0.0001;
  check_distrib 1000000 10 0.001;
  check_distrib 1000000 10 0.01;
  check_distrib 100000 10 0.1;
  check_distrib 100000 10 0.9

let[@inline never] check_callstack () =
  Printf.printf "check_callstack\n%!";
  let callstack = ref None in
  start {
      sampling_rate = 1.;
      callstack_size = 10;
      callback = fun info ->
        if info.tag = 0 then callstack := Some info.callstack;
        None
    };
  allocate_lists 1000000 1;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () =
  check_callstack ()

let () =
  Printf.printf "OK !\n"
