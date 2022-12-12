(* TEST
include runtime_events
*)
open Runtime_events

let majors = Atomic.make 0
let minors = Atomic.make 0

let got_start = ref false

let lost_events_count = ref 0

let lost_events domain_id num_events =
    lost_events_count := !lost_events_count + num_events

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

type phase_record = { mutable major: int; mutable minor: int }

let domain_tbl : (int, phase_record) Hashtbl.t = Hashtbl.create 5

let find_or_create_phase_count domain_id =
        match Hashtbl.find_opt domain_tbl domain_id with
        | None ->
            begin
                let new_count = { major = 0; minor = 0} in
                Hashtbl.add domain_tbl domain_id new_count;
                new_count
            end
        | Some(pc) -> pc

let runtime_begin domain_id ts phase =
    let phase_count = find_or_create_phase_count domain_id in
    match phase with
    | EV_MAJOR ->
        begin
            assert(phase_count.major >= 0);
            phase_count.major <- phase_count.major + 1;
        end
    | EV_MINOR ->
        begin
            assert(phase_count.minor == 0);
            phase_count.minor <- 1
        end
    | _ -> ()

let runtime_end domain_id ts phase =
    let phase_count = find_or_create_phase_count domain_id in
    match phase with
        | EV_MAJOR ->
            begin
                assert(phase_count.major >= 1);
                phase_count.major <- phase_count.major - 1;
                Atomic.incr majors
            end
        | EV_MINOR ->
            begin
                assert(phase_count.minor == 1);
                phase_count.minor <- 0;
                Atomic.incr minors
            end
        | _ -> ()
let num_domains = 3
let num_minors = 30

let () =
    start ();
    let cursor = create_cursor None in
    let gc_churn_f () =
        let list_ref = ref [] in
        for j = 0 to num_minors do
            list_ref := [];
            for a = 0 to 100 do
                list_ref := (Sys.opaque_identity(ref 42)) :: !list_ref
            done;
            Gc.minor ();
        done
    in
    let domains_list = List.init num_domains (fun _ -> Domain.spawn gc_churn_f) in
    let _ = List.iter Domain.join domains_list in
    let callbacks = Callbacks.create ~runtime_begin ~runtime_end ~lifecycle
                                        ~lost_events () in
    ignore(read_poll cursor callbacks None);
    assert(!got_start);
    assert(Atomic.get minors >= num_minors);
    assert(!lost_events_count == 0)
