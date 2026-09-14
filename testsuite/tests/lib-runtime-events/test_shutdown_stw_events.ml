(* TEST
 include runtime_events;
 include unix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 hasunix;
 not target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* Tests that when the runtime events ring is torn down with several domains
   running, no domain is left with an open EV_STW_LEADER or EV_STW_HANDLER
   event. The teardown runs in a stop-the-world section, so each domain is
   inside one of these when the ring goes away. *)

let domains = 3

let () =
  let parent_cwd = Sys.getcwd () in
  let child_pid = Unix.fork () in
  if child_pid == 0 then begin
    (* Child: start the ring, spawn domains that keep running, and exit, so
       that the teardown has several participants. *)
    Runtime_events.start ();
    let started = Atomic.make 0 in
    for _ = 1 to domains do
      ignore (Domain.spawn (fun () ->
        Atomic.incr started;
        (* cpu_relax services STW requests without allocating, so the ring
           does not fill up. *)
        while true do Domain.cpu_relax () done))
    done;
    while Atomic.get started < domains do Domain.cpu_relax () done;
    exit 0
  end else begin
    (* Parent: wait for the child, then read its preserved ring. *)
    let (_, status) = Unix.waitpid [] child_pid in
    assert (status = Unix.WEXITED 0);
    let cursor =
        Runtime_events.create_cursor (Some (parent_cwd, child_pid)) in
    (* For each ring and phase, whether the last event seen was a begin. Only
       the last event matters, so this is not confused if the ring wrapped
       and the oldest events were lost. *)
    let open_ = Hashtbl.create 16 in
    let runtime_begin ring _ phase = Hashtbl.replace open_ (ring, phase) true in
    let runtime_end ring _ phase = Hashtbl.replace open_ (ring, phase) false in
    let callbacks =
      Runtime_events.Callbacks.create ~runtime_begin ~runtime_end () in
    let rec drain () =
      if Runtime_events.read_poll cursor callbacks None > 0 then drain () in
    drain ();
    Runtime_events.free_cursor cursor;
    Unix.unlink
      (Filename.concat parent_cwd (string_of_int child_pid ^ ".events"));
    let unclosed phase =
      Hashtbl.fold (fun (_, p) is_open n ->
        if p = phase && is_open then n + 1 else n) open_ 0 in
    assert (unclosed Runtime_events.EV_STW_LEADER = 0);
    assert (unclosed Runtime_events.EV_STW_HANDLER = 0);
    (* Check the spawned domains' rings were read, so the test cannot pass
       by seeing only the main domain. *)
    let seen phase =
      Hashtbl.fold (fun (_, p) _ n -> if p = phase then n + 1 else n) open_ 0 in
    assert (seen Runtime_events.EV_STW_HANDLER >= domains);
    print_string "passed\n"
  end
