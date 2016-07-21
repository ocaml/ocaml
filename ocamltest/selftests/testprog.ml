(* Print messages on stdout and stderr to test redirections *)

open Printf

let _ =
  printf "stdout, message 1\n%!";
  eprintf "stderr, message 1\n%!";
  printf "stdout, message 2\n%!";
  eprintf "stderr, message 2\n%!"
