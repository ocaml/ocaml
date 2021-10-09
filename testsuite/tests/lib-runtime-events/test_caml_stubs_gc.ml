(* TEST
include runtime_events
*)
open Runtime_events

let got_minor = ref false

let () =
    start ();
    let cursor = create_cursor None in
    let runtime_begin domain_id ts phase =
      match phase with
      | EV_MINOR ->
        Gc.full_major ();
        got_minor := true
      | _ -> () in
    let callbacks = Callbacks.create ~runtime_begin ()
    in
    Gc.full_major ();
    ignore(read_poll cursor callbacks (Some 1_000));
    assert(!got_minor)
