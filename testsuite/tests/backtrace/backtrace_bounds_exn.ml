(* TEST
   flags = "-g"
   ocamlrunparam += ",b=1"
*)

(* bad bounds exception *)

let xs = [| 0; 1; 2 |]

let bad_bound_fn x =
  !x + xs.(100)

let _ =
  try
    ignore (Sys.opaque_identity (bad_bound_fn (ref 0)));
  with exn ->
    Printf.printf "Uncaught exception %s\n" (Printexc.to_string exn);
    Printexc.print_backtrace stdout;

  print_endline "OK"
