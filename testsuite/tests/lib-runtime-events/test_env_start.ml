(* TEST
include runtime_events
set OCAML_RUNTIME_EVENTS_START = "1"
*)

(* In this test the runtime_events should already be started by the environment
   variable that we are passing and so we should not need to start it *)

let got_start = ref false

let lifecycle domain_id ts lifecycle_event data =
  match lifecycle_event with
  | Runtime_events.EV_RING_START ->
      begin
          assert(match data with
          | Some(pid) -> true
          | None -> false);
          got_start := true
      end
  | _ -> ()

let () =
  let cursor = Runtime_events.create_cursor None in
    let callbacks = Runtime_events.Callbacks.create ~lifecycle () in
      let _read = Runtime_events.read_poll cursor callbacks None in
        assert(!got_start)
