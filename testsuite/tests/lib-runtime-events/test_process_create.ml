(* TEST
 include runtime_events;
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

(* Unix.create_process emits EV_PROCESS_CREATE. Unlike EV_FORK_PARENT this is
   expected to work on every platform. *)

let got_process_create = ref false
let event_pid = ref 0

let lifecycle _domain_index _ts lifecycle_event data =
  match lifecycle_event with
  | Runtime_events.EV_PROCESS_CREATE ->
      begin match data with
      | Some pid ->
          event_pid := pid;
          got_process_create := true
      | None -> assert false
      end
  | _ -> ()

(* Read until end of file and discard, so that whatever the child writes stays
   out of this test's own output. *)
let drain fd =
  let buf = Bytes.create 1024 in
  while Unix.read fd buf 0 1024 > 0 do () done

let () =
  Runtime_events.start ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create ~lifecycle () in
  let rd, wr = Unix.pipe () in
  let pid = Unix.create_process "cp" [| "cp" |] Unix.stdin wr wr in
  Unix.close wr;
  drain rd;
  Unix.close rd;
  ignore (Unix.waitpid [] pid);
  ignore (Runtime_events.read_poll cursor callbacks None);
  assert !got_process_create;
  assert (!event_pid > 0);
  (* On Windows Unix.create_process returns the process handle as a pseudo-pid
     while the event carries the real process id, so the two agree only on
     Unix. *)
  if Sys.os_type <> "Win32" then assert (!event_pid = pid);
  print_string "passed\n"
