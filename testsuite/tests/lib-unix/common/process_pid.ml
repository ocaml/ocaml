(* TEST
* hasunix
include unix
** bytecode
** native
*)

let null =
  if Sys.win32 then
    "NUL"
  else
    "/dev/null"

let () =
  let ic, _ as process =
    (* Redirect to null to avoid
       "The process tried to write to a nonexistent pipe." on Windows *)
    Printf.ksprintf Unix.open_process "echo toto > %s" null
  in
  assert
    (Unix.process_pid process = Unix.process_pid process);

  ignore (Unix.close_process process);
  print_endline "OK"
