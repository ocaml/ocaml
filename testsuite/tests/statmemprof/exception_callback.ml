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
  start {
    (* FIXME: we should use 1. to make sure the block is sampled,
       but the runtime does an infinite loop in native mode in this
       case. This bug will go away when the sampling of natively
       allocated will be correctly implemented. *)
    sampling_rate = 0.5;
    callstack_size = 10;
    callback = fun _ -> assert false
  };
  ignore (Sys.opaque_identity (Array.make 200 0));
  stop ()
