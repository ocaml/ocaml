(* TEST
set OCAML_EVENTRING_START = "1"
*)

(* In this test the eventring should already be started by the environment
   variable that we are passing and so we should not need to start it *)

let got_start = ref false

let lifecycle domain_id ts lifecycle_event data =
  match lifecycle_event with
  | Eventring.EV_RING_START ->
      begin
          assert(match data with
          | Some(pid) -> true
          | None -> false);
          got_start := true
      end
  | _ -> ()

let () =
  let cursor = Eventring.create_cursor None in
    let callbacks = Eventring.Callbacks.create ~lifecycle () in
      let _read = Eventring.read_poll cursor callbacks None in
        assert(!got_start)
