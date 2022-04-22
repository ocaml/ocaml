(* TEST
modules = "stubs.c"
include runtime_events
*)

external start_runtime_events : unit -> unit = "start_runtime_events"
external get_event_counts : unit -> (int * int * int) = "get_event_counts"

let () =
    start_runtime_events ();
    for a = 0 to 2 do
        ignore(Sys.opaque_identity(ref 42));
        Gc.compact ()
    done;
    let (minors, majors, compacts) = get_event_counts () in
    Printf.printf "minors: %d, majors: %d, compact: %d\n" minors majors compacts;
    (* Now test we can pause/resume while we're doing things *)
    for a = 0 to 2 do
        ignore(Sys.opaque_identity(ref 42));
        Runtime_events.resume ();
        Gc.compact ();
        Runtime_events.pause ()
    done;
    Printf.printf "minors: %d, majors: %d, compact: %d\n" minors majors compacts
