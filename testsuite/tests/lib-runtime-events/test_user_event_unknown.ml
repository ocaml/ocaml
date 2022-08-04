(* TEST
   include runtime_events
   include unix
   * libunix
   ** bytecode
   ** native
*)
open Runtime_events

type _ User.tag += Ev : int -> int User.tag

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
    User.write ev0 17;
    User.write ev1 12;
    User.write ev2 28;
    Unix.sleepf 0.2
  end
  else
  (* parent consumes events *)
  begin
  Unix.sleepf 0.1;
  let cursor = create_cursor (Some (parent_cwd, child_pid)) in
  let callback _ _ ev v =
    match User.tag ev with
    | Ev i -> Printf.printf "known event ev %d => %d\n" i v
    | _ -> Printf.printf "unknown event %s => %d\n" (User.name ev) v
  in
  let callbacks =
    Callbacks.create ()
    |> Callbacks.add Type.counter callback
  in
  for _ = 0 to 10 do
    read_poll cursor callbacks None |> ignore
  done
  end
