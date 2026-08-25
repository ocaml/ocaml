(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type foo = { mutable y:int }
let f (r: int) = r.y <- 3

let _ = { true with contents = 0 }

let t : string = 10
