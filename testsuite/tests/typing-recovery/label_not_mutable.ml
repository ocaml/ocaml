(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = { x: int; y: int; mutable z: int}

let f (x: t) =
  let () = x.z <- 11 in
  let () = x.x <- 12 in
  let () = x.y <- 12 in
  x

let g x y z = {x; y; z}

let t : string = 10
