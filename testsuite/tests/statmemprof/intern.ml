(* TEST
   flags = "-g"
   * bytecode
     reference = "${test_source_directory}/intern.byte.reference"
   * native
     reference = "${test_source_directory}/intern.opt.reference"
     compare_programs = "false"
*)

open Gc.Memprof

type t = Dummy of int (* Skip tag 0. *) | I of int | II of int * int | Cons of t
let rec t_of_len = function
  | len when len <= 1 -> assert false
  | 2 -> I 1
  | 3 -> II (2, 3)
  | len -> Cons (t_of_len (len - 2))

let marshalled_data = Hashtbl.create 17
let[@inline never] get_marshalled_data len : t =
  Marshal.from_string (Hashtbl.find marshalled_data len) 0
let precompute_marshalled_data lo hi =
  for len = lo to hi do
    if not (Hashtbl.mem marshalled_data len) then
      Hashtbl.add marshalled_data len (Marshal.to_string (t_of_len len) [])
  done

let root = ref []
let[@inline never] do_intern lo hi cnt keep =
  for j = 0 to cnt-1 do
    for i = lo to hi do
      root := get_marshalled_data i :: !root
    done;
    if not keep then root := []
  done

let check_nosample () =
  Printf.printf "check_nosample\n%!";
  precompute_marshalled_data 2 3000;
  start {
      sampling_rate = 0.;
      callstack_size = 10;
      callback = fun _ ->
        Printf.printf "Callback called with sampling_rate = 0\n";
        assert(false)
  };
  do_intern 2 3000 1 false

let () = check_nosample ()

let check_ephe_full_major () =
  Printf.printf "check_ephe_full_major\n%!";
  precompute_marshalled_data 2 3000;
  let ephes = ref [] in
  start {
    sampling_rate = 0.01;
    callstack_size = 10;
    callback = fun _ ->
      let res = Ephemeron.K1.create () in
      ephes := res :: !ephes;
      Some res
  };
  do_intern 2 3000 1 true;
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
  precompute_marshalled_data 2 300;
  let in_callback = ref false in
  start {
      sampling_rate = 1.;
      callstack_size = 10;
      callback = fun _ ->
        assert (not !in_callback);
        in_callback := true;
        do_intern 100 200 1 false;
        in_callback := false;
        None
  };
  do_intern 100 200 1 false;
  stop ()

let () = check_no_nested ()

let check_distrib lo hi cnt rate =
  Printf.printf "check_distrib %d %d %d %f\n%!" lo hi cnt rate;
  precompute_marshalled_data lo hi;
  let smp = ref 0 in
  start {
      sampling_rate = rate;
      callstack_size = 10;
      callback = fun info ->
        (* We also allocate the list constructor in the minor heap. *)
        if info.kind = Unmarshalled then begin
          begin match info.tag, info.size with
          | 1, 1 | 2, 2 | 3, 1 -> ()
          | _ -> assert false
          end;
          assert (info.n_samples > 0);
          smp := !smp + info.n_samples
        end;
        None
    };
  do_intern lo hi cnt false;
  stop ();

  (* The probability distribution of the number of samples follows a
     binomial distribution of parameters tot_alloc and rate. Given
     that tot_alloc*rate and tot_alloc*(1-rate) are large (i.e., >
     100), this distribution is approximately equal to a normal
     distribution. We compute a 1e-8 confidence interval for !smp
     using quantiles of the normal distribution, and check that we are
     in this confidence interval. *)
  let tot_alloc = cnt*(lo+hi)*(hi-lo+1)/2 in
  assert (float tot_alloc *. rate > 100. &&
          float tot_alloc *. (1. -. rate) > 100.);
  let mean = float tot_alloc *. rate in
  let stddev = sqrt (float tot_alloc *. rate *. (1. -. rate)) in
  (* This assertion has probability to fail close to 1e-8. *)
  assert (abs_float (mean -. float !smp) <= stddev *. 5.7)

let () =
  check_distrib 2 3000 3 0.00001;
  check_distrib 2 3000 1 0.0001;
  check_distrib 2 2000 1 0.01;
  check_distrib 2 2000 1 0.9;
  check_distrib 300000 300000 20 0.1

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
  precompute_marshalled_data 2 300;
  let callstack = ref None in
  start {
      sampling_rate = 1.;
      callstack_size = 10;
      callback = fun info ->
        if info.kind = Unmarshalled then callstack := Some info.callstack;
        None
    };
  do_intern 2 300 1 false;
  stop ();
  match !callstack with
  | None -> assert false
  | Some cs -> Printexc.print_raw_backtrace stdout cs

let () = check_callstack ()

let () =
  Printf.printf "OK !\n"
