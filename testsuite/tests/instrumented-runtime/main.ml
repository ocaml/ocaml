(* TEST
  * instrumented-runtime
  * skip
  reason = "instrumented runtime test is not very useful and broken on multicore. (#9413)"
  ** native
    flags = "-runtime-variant=i"
*)

(* Test if the instrumented runtime is in working condition *)

let _ =
  Gc.eventlog_pause ();
  Gc.eventlog_resume()
