(* TEST
* hasunix
include unix
*)

let () =
  let fd = Unix.openfile "plop" [O_CREAT; O_WRONLY] 0o666 in
  let pid =
    Unix.create_process "echo" [|"echo"; "toto"|] Unix.stdin fd Unix.stderr
  in
  Unix.close fd;
  while fst (Unix.waitpid [WNOHANG] pid) = 0 do
    Unix.sleepf 0.001
  done;
  match Sys.remove "plop" with
  | () ->  print_endline "OK"
  | exception (Sys_error _) -> print_endline "ERROR"

let () =
  let fd = Unix.openfile "plip" [O_CREAT; O_WRONLY] 0o666 in
  let pid =
    Unix.create_process "sleep" [|"sleep"; "0.01"|] Unix.stdin fd Unix.stderr
  in
  Unix.close fd;
  let pid, status = Unix.waitpid [WNOHANG] pid in
  assert (pid = 0);
  assert (status = WEXITED 0);
  while fst (Unix.waitpid [WNOHANG] pid) = 0 do
    Unix.sleepf 0.001
  done;
  match Sys.remove "plip" with
  | () ->  print_endline "OK"
  | exception (Sys_error _) -> print_endline "ERROR"
