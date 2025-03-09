(* TEST
 include runtime_events;
*)

(* Tests that:
 * - the runtime events subsystem works, logging events and passing
 *   them to OCaml;
 * - runtime event phases start and stop in matched pairs;
 * - major and minor collections happen at expected frequencies.
 *)

open Runtime_events

(* Record whether we have seen an EV_RING_START lifecycle event. *)

let got_start = ref false

let lifecycle domain_id ts lifecycle_event data =
    match lifecycle_event with
    | EV_RING_START ->
        begin
            assert (match data with
                    | Some(pid) -> true
                    | None -> false);
            got_start := true
        end
    | _ -> ()

(* Count completed major and minor collections, and check that phase
 * begin/end callbacks are called in order. *)

let in_major = ref false
let in_minor = ref false
let majors = ref 0
let minors = ref 0

let runtime_begin domain_id ts phase =
    match phase with
    | EV_MAJOR_FINISH_CYCLE ->
        begin
            assert (not !in_major);
            in_major := true
        end
    | EV_MINOR ->
        begin
            assert (not !in_minor);
            in_minor := true
        end
    | _ -> ()

let runtime_end domain_id ts phase =
    match phase with
    | EV_MAJOR_FINISH_CYCLE ->
        begin
            assert !in_major;
            in_major := false;
            incr majors
        end
    | EV_MINOR ->
        begin
            assert !in_minor;
            in_minor := false;
            incr minors
        end
    | _ -> ()

(* Record when unprocessed events are lost. If you see these messages,
 * it probably means your ring buffers are too small; try changing
 * the ocamlrunparam e=N setting at the head of this file.
 * Note that we only process events at the end of each "epoch" of
 * the test. *)

let lost_events domain_id num =
    Printf.printf "Lost %d events\n" num

let epochs = 20
let majors_per_epoch = 50
let conses_per_major = 10

let () =
    Gc.full_major ();
    let list_ref = ref [] in
    start ();
    let cursor = create_cursor None in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end ~lifecycle
                                    ~lost_events ()
    in
    for epoch = 1 to epochs do
        for major = 1 to majors_per_epoch do
            list_ref := [];
            for cons = 1 to conses_per_major do
                list_ref := (Sys.opaque_identity(ref cons)) :: !list_ref
            done;
            Gc.full_major ()
        done;
        ignore(read_poll cursor callbacks None)
    done;
    assert !got_start;
    Printf.printf "minors: %d, major cycles: %d\n" !minors !majors
