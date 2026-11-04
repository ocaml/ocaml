(* TEST
 hasunix;
 include runtime_events;
 include unix;
 {
   bytecode;
 }{
   native;
 }
*)

let run_child chdir =
  if chdir then
    Unix.chdir (Filename.get_temp_dir_name ());
  Runtime_events.create_cursor None |> Runtime_events.free_cursor

let () =
  Runtime_events.start ();
  [ false; true ] |> List.iter @@ fun chdir ->
  run_child chdir
