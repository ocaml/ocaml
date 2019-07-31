(* TEST
   flags = "-g"
   compare_programs = "false"
*)

open Gc.Memprof

type 'a list2 =  (* A list type where [Cons] has tag 1 *)
  | Nil
  | Dummy of int
  | Cons of 'a * 'a list2

let root = ref Nil
let[@inline never] allocate_arrays lo hi cnt keep =
  assert (0 < lo && hi <= 250);  (* Fits in minor heap. *)
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := Cons (Array.make i 0, !root)
    done;
    if not keep then root := Nil
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
  allocate_arrays 1 250 100 false

let () = check_nosample ()

let check_ephe_full_major () =
  Printf.printf "check_ephe_full_major\n%!";
  let ephes = ref [] in
  start {
    sampling_rate = 0.01;
    callstack_size = 10;
    callback = fun s ->
      assert (s.tag = 0 || s.tag = 1);
      let res = Ephemeron.K1.create () in
      ephes := res :: !ephes;
      Some res
  };
  allocate_arrays 1 250 100 true;
  stop ();
  List.iter (fun e -> assert (Ephemeron.K1.check_key e)) !ephes;
  Gc.full_major ();
  List.iter (fun e -> assert (Ephemeron.K1.check_key e)) !ephes;
  root := Nil;
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
        allocate_arrays 1 100 10 false;
        ignore (Array.to_list (Array.make 1000 0));
        in_callback := false;
        None
  };
  allocate_arrays 1 250 5 false;
  stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  let smp = ref 0 in
  start {
      sampling_rate = rate;
      callstack_size = 10;
      callback = fun info ->
        assert (info.kind = Minor);
        (* Exclude noise such as spurious closures and the root list. *)
        if info.tag = 0 then begin
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
  check_distrib 1 250 1000 0.00001;
  check_distrib 1 250 1000 0.0001;
  check_distrib 1 250 1000 0.01;
  check_distrib 1 250 1000 0.9;
  check_distrib 1 1   10000000 0.01;
  check_distrib 250 250 100000 0.1

(* FIXME : in bytecode mode, the function [caml_get_current_callstack_impl],
   which is supposed to capture the current call stack, does not have access
   to the current value of [pc]. Therefore, depending on how the C call is
   performed, we may miss the first call stack slot in the captured backtraces.
   This is the reason why the reference file is different in native and
   bytecode modes.

   Note that [Printexc.get_callstack] does not suffer from this problem, because
   this function is actually an automatically generated stub which performs th
   C call. This is because [Printexc.get_callstack] is not declared as external
   in the mli file. *)

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
  allocate_arrays 250 250 100 false;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () = check_callstack ()

let () =
  Printf.printf "OK !\n"
