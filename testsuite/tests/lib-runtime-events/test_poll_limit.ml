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

type Runtime_events.User.tag += T

let ev = Runtime_events.(User.register "u" T Type.unit)

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
  with Invalid_argument _ -> ();
  (* create some more events *)
  Gc.full_major ();
  let n : int = Runtime_events.read_poll cursor callbacks (Some 0) in
  assert (n = 0);
  let n : int = Runtime_events.read_poll cursor callbacks None in
  assert (n > 0);

  Runtime_events.User.write ev ();
  Runtime_events.User.write ev ();
  let n = Runtime_events.read_poll cursor callbacks (Some 1) in
  assert (n = 1)
