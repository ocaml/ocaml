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

  external pid_of_process : int -> int = "caml_test_pid_of_process"

  (* The child is this same program re-executed with an argument, rather than
     a fork, so that the test runs on Windows too. *)
  let () =
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then begin
      (* we are in the child, so start Runtime_events *)
      Runtime_events.start ();
      (* this creates a ring buffer. Now exit. *)
    end else begin
      let parent_cwd = Sys.getcwd () in
      let child_pid =
        Unix.create_process Sys.executable_name
          [| Sys.executable_name; "child" |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      (* Unix.waitpid closes the handle on Windows, so translate it first *)
      let child = pid_of_process child_pid in
      (* now wait for our child to finish *)
      Unix.waitpid [] child_pid |> ignore;
      (* child has finished. Is the file there? *)
      let cursor =
          Runtime_events.create_cursor (Some (parent_cwd, child)) in
      Runtime_events.free_cursor cursor;
      let ring_file =
          Filename.concat parent_cwd (string_of_int child ^ ".events") in
      Unix.unlink ring_file
    end
