(* TEST
 modules = "stubs.c";
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* On Windows, both Ctrl-C and Ctrl-Break are reported to OCaml programs
   as [Sys.sigint] (see [caml_win32_signal] in runtime/win32.c).

   OCaml 4.14 did this, but from 5.0 [Sys.set_signal] used the C
   runtime's [signal()] directly. That maps Ctrl-Break to SIGBREAK,
   which OCaml does not know about, so Ctrl-Break terminated the
   program even when a [Sys.sigint] handler was installed or
   [Sys.catch_break true] was in effect. *)

external use_private_console : unit -> bool = "caml_test_use_private_console"
external generate_ctrl_event : bool -> bool = "caml_test_generate_ctrl_event"

(* Console control handlers run asynchronously, on a thread created by
   the system, so spin (allocating, to reach polling points) until
   [cond] holds. *)
let wait_until cond =
  let deadline = Sys.time () +. 10. in
  while not (cond ()) && Sys.time () < deadline do
    ignore (Sys.opaque_identity (ref ()))
  done;
  cond ()

let event_name break = if break then "Ctrl-Break" else "Ctrl-C"

let test_handler ~break =
  let received = ref None in
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun s -> received := Some s));
  assert (generate_ctrl_event break);
  if wait_until (fun () -> !received <> None) then
    Printf.printf "%s: handler called with %s\n%!" (event_name break)
      (Sys.signal_to_string (Option.get !received))
  else
    Printf.printf "%s: handler not called\n%!" (event_name break);
  Sys.set_signal Sys.sigint Sys.Signal_default

let test_catch_break ~break =
  Sys.catch_break true;
  (match
     assert (generate_ctrl_event break);
     wait_until (fun () -> false)
   with
   | _ -> Printf.printf "%s: Sys.Break not raised\n%!" (event_name break)
   | exception Sys.Break ->
       Printf.printf "%s: Sys.Break raised\n%!" (event_name break));
  Sys.catch_break false

let () =
  assert (use_private_console ());
  test_handler ~break:false;
  test_handler ~break:true;
  test_catch_break ~break:false;
  test_catch_break ~break:true
