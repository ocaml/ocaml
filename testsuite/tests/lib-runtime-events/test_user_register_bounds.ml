(* TEST
 include runtime_events;
*)

(* Registering more than RUNTIME_EVENTS_MAX_CUSTOM_EVENTS (8192) custom events
   must raise rather than write the event name past the end of the mapped ring
   buffer. Regression test for the off-by-one in #15328. *)

open Runtime_events

type User.tag += T

let () =
  start ();
  (* indices 0 .. 8191 are the valid slots *)
  for i = 0 to 8191 do
    ignore (User.register (Printf.sprintf "ev%d" i) T Type.unit)
  done;
  match User.register "overflow" T Type.unit with
  | _ -> assert false
  | exception Invalid_argument _ -> ()
