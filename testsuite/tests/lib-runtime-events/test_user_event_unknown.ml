(* TEST
   include runtime_events
   include unix
   * libunix
   ** bytecode
   ** native
*)
open Runtime_events

type User.tag += Ev of int

let ev0 =
  User.register "ev0" (Ev 0) Type.counter

let () =
  start ();
  let parent_cwd = Sys.getcwd () in
  let child_pid = Unix.fork () in
  (* child generates events*)
  if child_pid == 0 then
  begin
    let ev1 = User.register "ev1" (Ev 1) Type.counter in
    let ev2 = User.register "ev2" (Ev 2) Type.counter in
    let ev3 = User.register "ev3" (Ev 3) Type.span in
    User.write ev0 17;
    User.write ev1 12;
    User.write ev2 28;
    User.write ev3 Begin;
    User.write ev3 End;
    Unix.sleepf 0.2
  end
  else
  (* parent consumes events *)
  begin
  Unix.sleepf 0.1;
  let cursor = create_cursor (Some (parent_cwd, child_pid)) in
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
    |> Callbacks.add_user_event Type.counter callback_counter
    |> Callbacks.add_user_event Type.span callback_span
  in
  for _ = 0 to 10 do
    read_poll cursor callbacks None |> ignore
  done
  end
