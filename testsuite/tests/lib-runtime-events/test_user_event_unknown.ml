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
open Runtime_events

external pid_of_process : int -> int = "caml_test_pid_of_process"

type User.tag += Ev of int

let ev0 =
  User.register "ev0" (Ev 0) Type.int

let custom_unit_type =
  Type.register ~encode:(fun buf () -> 0) ~decode:(fun buf sz -> ())

(* The child is this same program re-executed with an argument, rather than a
   fork, so that the test runs on Windows too. *)
let () =
  (* child generates events*)
  if Array.length Sys.argv > 1 && Sys.argv.(1) = "child" then
  begin
    start ();
    let ev1 = User.register "ev1" (Ev 1) Type.int in
    let ev2 = User.register "ev2" (Ev 2) Type.int in
    let ev3 = User.register "ev3" (Ev 3) Type.span in
    let ev4 = User.register "ev4" (Ev 4) custom_unit_type in

    User.write ev0 17;
    User.write ev1 12;
    User.write ev2 28;
    User.write ev3 Begin;
    User.write ev3 End;
    User.write ev4 ()
  end
  else
  (* parent consumes events *)
  begin
  let parent_cwd = Sys.getcwd () in
  let child_pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "child" |] Unix.stdin Unix.stdout Unix.stderr
  in
  (* Unix.waitpid closes the handle on Windows, so translate it first *)
  let child = pid_of_process child_pid in
  Unix.waitpid [] child_pid |> ignore;
  let cursor = create_cursor (Some (parent_cwd, child)) in
  let callback_counter _ _ ev v =
    match User.tag ev with
    | Ev i -> Printf.printf "known event ev %d => %d\n" i v
    | _ -> Printf.printf "unknown event %s => %d\n" (User.name ev) v
  in
  let callback_span _ _ ev v =
    Printf.printf "span %s => %b\n" (User.name ev) (v ==  Type.Begin)
  in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add_user_event Type.int callback_counter
    |> Callbacks.add_user_event Type.span callback_span
  in
  for _ = 0 to 10 do
    read_poll cursor callbacks None |> ignore
  done
  end
