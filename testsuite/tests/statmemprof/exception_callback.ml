(* TEST
   exit_status = "2"
*)

open Gc.Memprof

(* We don't want to print the backtrace. We just want to make sure the
   exception is printed.
   This also makes sure [Printexc] is loaded, otherwise we don't use
   its uncaught exception handler. *)
let _ = Printexc.record_backtrace false

let _ =
  start ~callstack_size:10
        ~minor_alloc_callback:(fun _ -> assert false)
        ~major_alloc_callback:(fun _ -> assert false)
        ~sampling_rate:1. ();
  ignore (Sys.opaque_identity (Array.make 200 0));
  stop ()
