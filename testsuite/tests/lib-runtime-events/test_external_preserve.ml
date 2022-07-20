(* TEST
  include runtime_events
  include unix
  set OCAML_RUNTIME_EVENTS_PRESERVE = "1"
  * libunix
  ** bytecode
  ** native *)

  (* this tests the preservation of ring buffers after termination *)

  let () =
    (* start runtime_events now to avoid a race *)
    let parent_cwd = Sys.getcwd () in
    let child_pid = Unix.fork () in
    if child_pid == 0 then begin
      (* we are in the child, so start Runtime_events *)
      Runtime_events.start ();
      (* this creates a ring buffer. Now exit. *)
    end else begin
      (* now wait for our child to finish *)
      Unix.wait () |> ignore;
      (* child has finished. Is the file there? *)
      let cursor =
          Runtime_events.create_cursor (Some (parent_cwd, child_pid)) in
      Runtime_events.free_cursor cursor;
      let ring_file =
          Filename.concat parent_cwd (string_of_int child_pid ^ ".events") in
      Unix.unlink ring_file
    end
