(* TEST
   modules = "stubs.c"
*)

external start_eventring : unit -> unit = "start_eventring"
external get_event_counts : unit -> (int * int * int) = "get_event_counts"

let () =
    start_eventring ();
    for a = 0 to 2 do
        ignore(Sys.opaque_identity(ref 42));
        Gc.compact ()
    done;
    let (minors, majors, compacts) = get_event_counts () in
    Printf.printf "minors: %d, majors: %d, compact: %d\n" minors majors compacts;
    (* Now test we can pause/resume while we're doing things *)
    for a = 0 to 2 do
        ignore(Sys.opaque_identity(ref 42));
        Eventring.resume ();
        Gc.compact ();
        Eventring.pause ()
    done;
    Printf.printf "minors: %d, majors: %d, compact: %d\n" minors majors compacts
