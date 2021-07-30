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
  start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with
      alloc_minor = (fun _ -> assert false);
      alloc_major = (fun _ -> assert false);
    };
  ignore (Sys.opaque_identity (ref (ref 42)));
  stop ()
