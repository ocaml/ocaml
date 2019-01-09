(* TEST
   flags = "-g"
   * hassysthreads
   include systhreads
   compare_programs = "false"
   ** no-flambda
   reference = "${test_source_directory}/callstack.reference"
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
let () = Printf.printf "new thread:\n"
let () = Thread.join (Thread.create f3 ())
