(* TEST
 include runtime_events;
*)
open Runtime_events

type User.tag += Buggy_decoder

exception Test_exception

let buggy =
  let encode buf _ = Bytes.set buf 0 'x'; 1
  and decode _ _ = raise Test_exception in
  Type.register ~encode ~decode

let event = User.register "buggy" Buggy_decoder buggy

let callbacks =
  Callbacks.add_user_event buggy (fun _ _ _ _ -> ()) (Callbacks.create ())

let () =
  start ();
  let cursor = create_cursor None in
  User.write event ();
  (try ignore (read_poll cursor callbacks None) with Test_exception -> ());
  (* The decoder exception must not leave cursor_in_poll set: before this was
     fixed the second poll failed with "poll called concurrently or reentrant"
     rather than surfacing the decoder's own error. *)
  match read_poll cursor callbacks None with
  | _ -> print_string "OK"
  | exception Test_exception -> print_string "OK"
  | exception e -> Printf.printf "FAIL: %s" (Printexc.to_string e)
