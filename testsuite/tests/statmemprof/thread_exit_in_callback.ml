(* TEST
modules = "thread_exit_in_callback_stub.c"
exit_status = "42"
* hassysthreads
include systhreads
** bytecode
** native
*)

(* We cannot tell Ocamltest that this program is supposed to stop with
   a fatal error. Instead, we install a fatal error hook and call exit(42) *)
external install_fatal_error_hook : unit -> unit = "install_fatal_error_hook"

let _ =
  install_fatal_error_hook ();
  Gc.Memprof.(start ~callstack_size:10 ~sampling_rate:1.
    { null_tracker with alloc_minor = fun _ -> Thread.exit (); None });
  ignore (Sys.opaque_identity (ref 1))
