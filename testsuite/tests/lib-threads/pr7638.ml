(* TEST

unset DOES_NOT_EXIST

* hassysthreads
include systhreads
** bytecode
** native

*)

(* MPR#7638 repro case *)

let crashme v =
  match Sys.getenv v with
  | exception Not_found -> print_string "OK\n"
  | s -> print_string "Surprising but OK\n"

let _ =
  let th = Thread.create crashme "DOES_NOT_EXIST" in
  Thread.join th
