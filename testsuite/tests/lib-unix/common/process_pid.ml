(* TEST
* hasunix
include unix
** bytecode
** native
*)

let () =
  let ic, _ as process =
    Unix.open_process "echo toto"
  in
  assert
    (Unix.process_pid process = Unix.process_pid process);

  (* read everything to avoid
     "The process tried to write to a nonexistent pipe." on Windows *)
  ignore (input_line ic);

  ignore (Unix.close_process process);
  print_endline "OK"
