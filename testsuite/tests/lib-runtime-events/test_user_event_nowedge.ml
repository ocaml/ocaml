(* TEST
 include runtime_events;
 include unix;
 hasunix;
 set OCAML_RUNTIME_EVENTS_PRESERVE = "1";
 not target-windows;
 {
   bytecode;
 }{
   native;
 }
*)
open Runtime_events

type User.tag += Ev of int

let () =
  let parent_cwd = Sys.getcwd () in
  let child_pid = Unix.fork () in
  (* child generates events*)
  if child_pid == 0 then
  begin
    start ();
    for i = 1 to 8192 do
      let ev = User.register (string_of_int i) (Ev i) Type.int in
      User.write ev i
    done
  end
  else
  (* parent consumes events *)
  begin
  Unix.waitpid [] child_pid |> ignore;
  let _ = User.register "test" (Ev 0) Type.int in
  let cursor = create_cursor (Some (parent_cwd, child_pid)) in
  let callback_counter _ _ ev v =
    ()
  in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add_user_event Type.int callback_counter
  in
  let finally () = Runtime_events.free_cursor cursor in
  Fun.protect ~finally @@ fun () ->
  let got = ref false in
  for _ = 0 to 8192 do
    try read_poll cursor callbacks None |> ignore
    with Invalid_argument _ -> got := true
  done;
  assert !got
  end
