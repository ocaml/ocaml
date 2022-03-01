(* TEST
   include unix
   * libunix
   ** bytecode
   ** native *)

let got_start = ref false
let got_fork_child = ref false
let got_fork_parent = ref false

let child_pid = ref 0
let am_child = ref false

let lifecycle domain_id ts lifecycle_event data =
    match lifecycle_event with
    | Eventring.EV_RING_START ->
        begin
            assert(match data with
            | Some(pid) -> true
            | None -> false);
            got_start := true
        end
    | Eventring.EV_FORK_PARENT ->
        begin
            (match data with
            | Some(pid) ->
                begin
                    child_pid := pid;
                    got_fork_parent := true
                end
            | None -> assert(false));
        end
    | Eventring.EV_FORK_CHILD ->
        got_fork_child := true
    | _ -> ()

let () =
    Eventring.start ();
    let new_child_pid = Unix.fork () in
    let cursor = Eventring.create_cursor None in
    let callbacks = Eventring.Callbacks.create ~lifecycle () in
    ignore(Eventring.read_poll cursor callbacks None);
    if new_child_pid == 0 then
        assert(!got_fork_child)
    else
        assert(!got_fork_parent && !child_pid > 0);
    assert(!got_start);
