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

let () =
  Runtime_events.start ();
  (* create some events *)
  Gc.full_major ();
  let cursor = Runtime_events.create_cursor None in
  let callbacks = Runtime_events.Callbacks.create () in
  for n = 1 to 100 do
    let processed = Runtime_events.read_poll cursor callbacks (Some n) in
    assert (processed <= n)
  done;
  try
    let _ : int = Runtime_events.read_poll cursor callbacks (Some ~-1) in
    assert false
  with Invalid_argument _ -> ()
