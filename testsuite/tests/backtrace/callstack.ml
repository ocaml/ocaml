(* TEST
   flags = "-g"
   * hassysthreads
   include systhreads
   compare_programs = "false"
   ** no-flambda
   *** native
   *** bytecode
*)

let[@inline never] f0 () =
  Printexc.print_raw_backtrace stdout (Printexc.get_callstack 100); ()
let[@inline never] f1 () = f0 (); ()
let[@inline never] f2 () = f1 (); ()
let[@inline never] f3 () = f2 (); ()

let () = Printf.printf "main thread:\n"
let () = f3 ()

let () = Printf.printf "from finalizer:\n"
let () =
  Gc.finalise (fun _ -> f0 ()) [|1|];
  Gc.full_major ();
  ()

(* We run this last, because the initialization of the thread library
   starts the "tick thread", which periodically send a signal for
   thread preemption. If the preempion occurs exactly when the
   finalizer above runs, then a new row for [Thread.yield] appears in
   the callstack, which breaks the test. *)
let () = Printf.printf "new thread:\n"
let () = Thread.join (Thread.create f3 ())
