(* TEST
include runtime_events
*)
open Runtime_events

let counters_tbl = Hashtbl.create 50

let runtime_counter _domain_id ts name value =
    Hashtbl.add counters_tbl name value

let () =
    start ();
    let cursor = create_cursor None in
    let callbacks = Callbacks.create ~runtime_counter () in
    Gc.set { (Gc.get()) with Gc.minor_heap_size = 65536 };
    Gc.full_major ();
    ignore(read_poll cursor callbacks None);
    let received ev = Hashtbl.find_opt counters_tbl ev |> Option.is_some in
    (* Test the minor heap size change we did earlier is there *)
    assert(received EV_C_FORCE_MINOR_SET_MINOR_HEAP_SIZE);
    (* Now all the major heap sizings *)
    assert(received EV_C_MAJOR_HEAP_POOL_WORDS);
    assert(received EV_C_MAJOR_HEAP_POOL_LIVE_WORDS);
    assert(received EV_C_MAJOR_HEAP_POOL_FRAG_WORDS);
    assert(received EV_C_MAJOR_HEAP_POOL_LIVE_BLOCKS);
    assert(received EV_C_MAJOR_HEAP_LARGE_WORDS);
    assert(received EV_C_MAJOR_HEAP_LARGE_BLOCKS);
    (* Finally the minor heap counters *)
    assert(received EV_C_MINOR_ALLOCATED);
    assert(received EV_C_MINOR_PROMOTED)
