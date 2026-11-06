(* TEST
   include runtime_events;
   include unix;
   hasunix;
   not windows;
   run_can_skip = "true";
   {
     bytecode;
   }
   {
     native;
   }
 *)

open Runtime_events

let attack_target = "important_file"

let symlink_attack pid =
  (* perform symlink attack, normally this would be performed
     by an external process that needs to win a race,
     or create lots of symlinks.
     For testing we create the symlink in the process directly
   *)
  Out_channel.with_open_text attack_target (fun ch -> Out_channel.output_string ch "test");
  let path = Printf.sprintf "%d.events" pid in
  let () =
    [ attack_target; path] |> List.iter @@ fun p ->
    try Unix.unlink p
    with Unix.Unix_error(Unix.ENOENT, _, _) -> ()
  in
  Unix.symlink attack_target path

let () =
  if not @@ Unix.has_symlink () then
    exit 125;
  symlink_attack (Unix.getpid ());
  start ();
  if Sys.file_exists attack_target then
    print_endline "Symlink attack succeeded"
