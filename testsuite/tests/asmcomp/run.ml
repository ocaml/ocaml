external run_prog : int -> int -> int -> unit = "run_prog"

let arg n =
  if n < Array.length Sys.argv then
    int_of_string Sys.argv.(n)
  else
    0

let () = run_prog (arg 1) (arg 2) (arg 3)
