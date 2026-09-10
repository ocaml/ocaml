(* TEST
 include runtime_events;
 include unix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 ocamlrunparam += ",c=1";
 hasunix;
 not target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* The last domain runs its final collections inside caml_domain_terminate,
   which happens after caml_do_exit has emitted the GC statistics. Tearing the
   ring down before then loses those events. The child writes a marker as the
   last thing it does itself, so any minor collection reported after the marker
   can only have come from the shutdown path.

   caml_domain_terminate is only reached for the initial domain when
   cleanup_on_exit is set. *)

type Runtime_events.User.tag += Marker

let seen_marker = ref false
let minors_after_marker = ref 0

let () =
  let parent_cwd = Sys.getcwd () in
  let child_pid = Unix.fork () in
  if child_pid == 0 then begin
    Runtime_events.start ();
    let marker =
      Runtime_events.User.register "shutdown.marker" Marker
        Runtime_events.Type.unit in
    (* Allocate so the shutdown collections have something to report. *)
    let r = ref [] in
    for _ = 1 to 20_000 do
      r := [| 1; 2; 3; 4 |] :: !r;
      if List.length !r > 100 then r := []
    done;
    Runtime_events.User.write marker ();
    exit 0
  end else begin
    Unix.wait () |> ignore;
    let cursor =
        Runtime_events.create_cursor (Some (parent_cwd, child_pid)) in
    let marker_cb _ _ _ () = seen_marker := true in
    let runtime_end _ _ phase =
      match phase with
      | Runtime_events.EV_MINOR ->
          if !seen_marker then incr minors_after_marker
      | _ -> () in
    let callbacks =
      Runtime_events.Callbacks.create ~runtime_end ()
      |> Runtime_events.Callbacks.add_user_event Runtime_events.Type.unit
           marker_cb in
    let rec drain () =
      if Runtime_events.read_poll cursor callbacks None > 0 then drain () in
    drain ();
    Runtime_events.free_cursor cursor;
    let ring_file =
        Filename.concat parent_cwd (string_of_int child_pid ^ ".events") in
    Unix.unlink ring_file;
    assert !seen_marker;
    assert (!minors_after_marker > 0);
    print_string "passed\n"
  end
