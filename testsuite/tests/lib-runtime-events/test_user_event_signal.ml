(* TEST
 include runtime_events;
 include unix;
 hasunix;
 not-windows;
 {
   bytecode;
 }
 {
   native;
 }
*)

(* Check that emitting a custom event from a signal handler works (see #12900).
 *)

let unit =
  let encode _buf () =
    print_endline "Start encoding event";
    Unix.sleepf 1.5;
    print_endline "Finished encoding event";
    0
  in
  let decode _buf _len = () in
  Runtime_events.Type.register ~encode ~decode

type Runtime_events.User.tag += My_event
let my_event = Runtime_events.User.register "event" My_event unit

let handle_signal _ =
  print_endline "Signal handler called; writing trace event...";
  Runtime_events.User.write my_event ()

let () =
  Runtime_events.start ();
  Sys.set_signal Sys.sigalrm (Signal_handle handle_signal);
  ignore (Unix.alarm 1 : int);
  Runtime_events.User.write my_event ();
  print_endline "Done"
