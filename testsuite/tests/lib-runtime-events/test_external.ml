(* TEST
 modules = "pid_stubs.c";
 include runtime_events;
 include unix;
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

let got_major = ref false
let got_minor = ref false
let finished = ref false

let runtime_end domain_id ts phase =
  match phase with
  | Runtime_events.EV_EXPLICIT_GC_FULL_MAJOR ->
    got_major := true
  | Runtime_events.EV_MINOR ->
    got_minor := true
  | _ -> ()

external get_current_process_id : unit -> int
  = "caml_test_get_current_process_id"

(* Re-exec instead of fork so this runs on Windows, passing the parent's pid
   since Unix.getppid is unavailable there. *)
let () =
  let parent_cwd = Sys.getcwd () in
  if Array.length Sys.argv > 2 && Sys.argv.(1) = "child" then begin
    (* we are in the child *)
    let parent_pid = int_of_string Sys.argv.(2) in
    let cursor = Runtime_events.create_cursor (Some (parent_cwd, parent_pid)) in
    let callbacks = Runtime_events.Callbacks.create ~runtime_end () in
    let started = Unix.gettimeofday () in
    while (not !finished) && (Unix.gettimeofday () -. started < 10.) do
      Runtime_events.read_poll cursor callbacks None |> ignore;
      if !got_major && !got_minor then
        finished := true
    done;
    assert(!got_minor);
    assert(!got_major);
  end else begin
    (* start runtime_events now to avoid a race *)
    Runtime_events.start ();
    let parent_pid = string_of_int (get_current_process_id ()) in
    let child_pid =
      Unix.create_process Sys.executable_name
        [| Sys.executable_name; "child"; parent_pid |]
        Unix.stdin Unix.stdout Unix.stderr
    in
    (* we are in the parent, generate some events *)
    Gc.full_major ();
    (* now wait for our child to finish *)
    Unix.waitpid [] child_pid |> ignore
  end
