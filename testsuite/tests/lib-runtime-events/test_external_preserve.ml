(* TEST
 modules = "pid_stubs.c";
 include runtime_events;
 include unix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 hasunix;
 {
   bytecode;
 }{
   native;
 }
*)

  (* this tests the preservation of ring buffers after termination *)

  external get_process_id : int -> int = "caml_test_get_process_id"

  (* Re-exec instead of fork so this runs on Windows. *)
  let () =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then begin
      (* we are in the child, so start Runtime_events *)
      Runtime_events.start ();
      (* this creates a ring buffer. Now exit. *)
    end else begin
      let parent_cwd = Sys.getcwd () in
      let child =
        Unix.create_process Sys.executable_name
          [| Sys.executable_name; "child" |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      let child_pid = get_process_id child in
      (* now wait for our child to finish *)
      Unix.waitpid [] child |> ignore;
      (* child has finished. Is the file there? *)
      let cursor =
          Runtime_events.create_cursor (Some (parent_cwd, child_pid)) in
      Runtime_events.free_cursor cursor;
      let ring_file =
          Filename.concat parent_cwd (string_of_int child_pid ^ ".events") in
      Unix.unlink ring_file
    end
