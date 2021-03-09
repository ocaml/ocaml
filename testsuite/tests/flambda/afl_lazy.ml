(* TEST
   * flambda
   ** native
   ocamlopt_flags = "-O3 -afl-instrument"
*)

let f l =
  Lazy.force l

let _ =
  Sys.opaque_identity (f (lazy "Hello"))
