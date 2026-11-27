(* TEST
 include runtime_events;
 include unix;
 hasunix;
 not target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

let run_child chdir =
  (* flush before forking, otherwise we get duplicate output *)
  flush_all ();
  let child_pid = Unix.fork () in
  if child_pid == 0 then begin
    Runtime_events.start ();
    if chdir then
      Unix.chdir (Filename.get_temp_dir_name ());
    exit 0
  end
  else begin
    (* wait for child *)
    Unix.wait () |> ignore;
    (* check ring file presence *)
    let path = Printf.sprintf "%d.events" child_pid in
    let exists = Sys.file_exists path in
    Printf.printf "chdir: %b, file exists after child exits: %b\n" chdir exists;
    (* the ring file should only exist on exit when preserve is set *)
    if exists then
      Printf.eprintf "ERROR: Ring file still exists\n"
  end

let () =
  [ false; true ] |> List.iter @@ fun chdir ->
  run_child chdir
