(* TEST
* hasunix
include unix
*)

let () =
  let process =
    Unix.open_process "echo toto"
  in
  assert
    (Unix.process_pid process = Unix.process_pid process);
  Unix.close_process process;
  print_endline "OK"
