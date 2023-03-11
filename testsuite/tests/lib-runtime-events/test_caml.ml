(* TEST
include runtime_events
*)
open Runtime_events

let major = ref 0
let minor = ref 0
let compact = ref 0
let majors = ref 0
let minors = ref 0
let compacts = ref 0

let got_start = ref false

let lifecycle domain_id ts lifecycle_event data =
    match lifecycle_event with
    | EV_RING_START ->
        begin
            assert(match data with
            | Some(pid) -> true
            | None -> false);
            got_start := true
        end
    | _ -> ()

let runtime_begin domain_id ts phase =
    match phase with
    | EV_MAJOR_FINISH_CYCLE ->
        begin
            assert(!major == 0);
            major := 1
        end
    | EV_MINOR ->
        begin
            assert(!minor == 0);
            minor := 1
        end
    | _ -> ()

let runtime_end domain_id ts phase =
    match phase with
    | EV_MAJOR_FINISH_CYCLE ->
        begin
            assert(!major == 1);
            major := 0;
            incr majors
        end
    | EV_MINOR ->
        begin
            assert(!minor == 1);
            minor := 0;
            incr minors
        end
    | _ -> ()

let lost_events domain_id num =
    Printf.printf "Lost %d events\n" num

let epochs = 20

let () =
    let list_ref = ref [] in
    start ();
    let cursor = create_cursor None in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end ~lifecycle
                                    ~lost_events ()
    in
    for epoch = 1 to epochs do
        for a = 1 to 100 do
            list_ref := [];
            for a = 1 to 10 do
                list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
            done;
            Gc.full_major ()
        done;
        ignore(read_poll cursor callbacks None)
    done;
    assert(!got_start);
    Printf.printf "minors: %d, major cycles: %d\n" !minors !majors
