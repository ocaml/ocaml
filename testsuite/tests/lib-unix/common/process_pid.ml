(* TEST
* hasunix
include unix
** bytecode
** native
*)

let () =
  let process =
    Unix.open_process "echo toto"
  in
  assert
    (Unix.process_pid process = Unix.process_pid process);
  ignore (Unix.close_process process);
  print_endline "OK"
