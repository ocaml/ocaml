(* TEST
   modules = "minor_no_postpone_stub.c"
*)

open Gc.Memprof

let notify_minor ref_ok ref_done =
  { null_tracker with
    alloc_minor = (fun _ ->
      assert !ref_ok;
      ref_done := true;
      None);
  }

let () =
  let callback_ok = ref true in
  let callback_done = ref false in
  start ~callstack_size:0 ~sampling_rate:1.
    (notify_minor callback_ok callback_done);
  ignore (Sys.opaque_identity (ref 0));
  assert(!callback_done);
  callback_ok := false;
  stop ()

external alloc_stub : unit -> unit ref = "alloc_stub"

let () =
  let callback_ok = ref false in
  let callback_done = ref false in
  start ~callstack_size:0 ~sampling_rate:1.
    (notify_minor callback_ok callback_done);
  ignore (Sys.opaque_identity (alloc_stub ()));
  assert(not !callback_done);
  callback_ok := true;
  stop ();
  assert(!callback_done)
