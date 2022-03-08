(* TEST
   include unix
   * libunix
   ** bytecode
   ** native *)

let got_major = ref false
let got_minor = ref false
let finished = ref false

let runtime_end domain_id ts phase =
  match phase with
  | Eventring.EV_EXPLICIT_GC_FULL_MAJOR ->
    got_major := true
  | Eventring.EV_MINOR ->
    got_minor := true
  | _ -> ()

let () =
  (* start eventring now to avoid a race *)
  Eventring.start ();
  let parent_pid = Unix.getpid () in
  let parent_cwd = Sys.getcwd () in
  let child_pid = Unix.fork () in
  if child_pid == 0 then begin
    (* we are in the child *)
    let cursor = Eventring.create_cursor (Some (parent_cwd, parent_pid)) in
    let callbacks = Eventring.Callbacks.create ~runtime_end () in
    let started = Unix.gettimeofday () in
    while (not !finished) && (Unix.gettimeofday () -. started < 10.) do
      Eventring.read_poll cursor callbacks None |> ignore;
      if !got_major && !got_minor then
        finished := true
    done;
    assert(!got_minor);
    assert(!got_major);
  end else begin
    (* we are in the parent, generate some events *)
    Gc.full_major ();
    (* now wait for our child to finish *)
    Unix.wait () |> ignore
  end
