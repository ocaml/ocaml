(* TEST
 include unix;
 include runtime_events;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

(* Tests that [create_cursor]:
 * - fails on [None] if runtime events haven't been started
 * - doesn't double-free when it fails to attach to [None]
 * - does manage to attach to this process if we provide the right PID
 *)

let create_and_free ?(pid) () =
  try
    let dir_and_pid = Option.map (fun p -> ".", p) pid in
    let cur = Runtime_events.create_cursor dir_and_pid in
    Runtime_events.free_cursor cur;
    print_endline "OK"
  with Failure msg -> print_endline msg

let start_and_pause () =
  Runtime_events.start ();
  Runtime_events.pause ()

(* workaround for finding the events file even on Windows, where
   [Unix.getpid] doesn't match the one used to open the file *)
let find_events_file =
  let list_events_files () =
    List.filter
      (String.ends_with ~suffix:".events")
      (Array.to_list @@ Sys.readdir ".")
  in
  let startup_events_files = list_events_files () in
  fun () ->
  let is_new_event_file f = not (List.mem f startup_events_files) in
  List.find is_new_event_file (list_events_files ())
(* once again, Windows workaround to get the correct PID *)
let find_events_pid () = Scanf.sscanf (find_events_file()) "%d.events" Fun.id

(* force failure of [create_cursor None] *)
let make_unreadable () = Unix.chmod (find_events_file()) 0o000

let () =
  create_and_free (); (* fail, not started *)
  start_and_pause ();
  let pid = find_events_pid () in
  create_and_free ~pid (); (* success *)
  create_and_free (); (* success *)
  make_unreadable ();
  create_and_free ~pid (); (* fail, cannot open *)
  create_and_free (); (* fail, cannot open *)
  create_and_free (); (* fail, cannot open *)
