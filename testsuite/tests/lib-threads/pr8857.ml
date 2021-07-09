(* TEST

* hassysthreads
include systhreads
** bytecode
** native

*)

let _ =
  try Unix.utimes "does-not-exist" 0.0 0.0
  with Unix.(Unix_error(ENOENT, _, _)) -> ()
