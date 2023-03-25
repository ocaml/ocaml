(* TEST
   include runtime_events
   include unix
   set OCAMLRUNPARAM = "e=6"
   * libunix
   ** native
   ** bytecode
*)

type Runtime_events.User.tag += Ev
let ev = Runtime_events.User.register "ev" Ev Runtime_events.Type.int

let producer () =
  let open Runtime_events in
  for _ = 0 to 100000 do
    User.write ev 0
  done

let ready = Atomic.make 0

let wait_ready () =
  let v = Atomic.fetch_and_add ready 1 in
  if v < 2 then
    while Atomic.get ready < 2 do
      Domain.cpu_relax ()
    done

let _ =
  Domain.spawn (fun () ->
    Runtime_events.start ();
    wait_ready ();
    producer ())

let callbacks =
  let open Runtime_events in
  let evs = Runtime_events.Callbacks.create ()
  in
  let id_callback d ts c i =
    assert (i = 0)
  in
  Callbacks.add_user_event Runtime_events.Type.int id_callback evs

let ()
 =
  Unix.sleepf 0.1;
  let cursor = Runtime_events.create_cursor None in
  wait_ready ();
  for _ = 0 to 10 do
    Runtime_events.read_poll cursor callbacks None
    |> ignore
  done
