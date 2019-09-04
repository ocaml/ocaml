(* TEST
   modules = "minor_no_postpone_stub.c"
   * bytecode
*)

open Gc.Memprof

let () =
  let callback_ok = ref true in
  let callback_done = ref false in
  start ~callstack_size:0
        ~minor_alloc_callback:(fun _ ->
          assert !callback_ok;
          callback_done := true;
          None)
        ~sampling_rate:1. ();
  ignore (Sys.opaque_identity (ref 0));
  assert(!callback_done);
  callback_ok := false;
  stop ()

external alloc_stub : unit -> unit ref = "alloc_stub"

let () =
  let callback_ok = ref false in
  let callback_done = ref false in
  start ~callstack_size:0
        ~minor_alloc_callback:(fun _ ->
          assert !callback_ok;
          callback_done := true;
          None)
        ~sampling_rate:1. ();
  ignore (Sys.opaque_identity (alloc_stub ()));
  assert(not !callback_done);
  callback_ok := true;
  stop ();
  assert(!callback_done)
