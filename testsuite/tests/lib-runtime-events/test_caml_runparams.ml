(* TEST
include runtime_events
ocamlrunparam += ",e=4"
*)

(* We set the ring buffer size smaller and witness that we do indeed
   lose events. *)
open Runtime_events

let lost_any_events = ref false

let lost_events _domain_id num =
    if num > 0 then
        lost_any_events := true

let () =
    start ();
    let cursor = create_cursor None in
    let callbacks = Callbacks.create ~lost_events ()
    in
    for epoch = 1 to 20 do
        Gc.full_major ()
    done;
    ignore(read_poll cursor callbacks None);
    assert(!lost_any_events)
