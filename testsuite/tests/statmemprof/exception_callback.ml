(* TEST
   exit_status = "2"
*)

open Gc.Memprof

let alloc_tracker on_alloc =
  { null_tracker with
    alloc_minor = (fun info -> on_alloc info; None);
    alloc_major = (fun info -> on_alloc info; None);
  }

(* We don't want to print the backtrace. We just want to make sure the
   exception is printed.
   This also makes sure [Printexc] is loaded, otherwise we don't use
   its uncaught exception handler. *)
let _ = Printexc.record_backtrace false

let _ =
  start ~callstack_size:10 ~sampling_rate:1.
    (alloc_tracker (fun _ -> failwith "callback failed"));
  ignore (Sys.opaque_identity (Array.make 200 0));
  stop ()
