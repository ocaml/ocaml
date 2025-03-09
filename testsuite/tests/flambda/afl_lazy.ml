(* TEST
 flambda;
 ocamlopt_flags = "-O3 -afl-instrument";
 native;
*)

let f l =
  Lazy.force l

let _ =
  Sys.opaque_identity (f (lazy "Hello"))
