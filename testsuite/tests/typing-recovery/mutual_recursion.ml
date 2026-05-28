(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let rec foo x = x + a + b
and (a,b) = (1,2)

let c = foo 10 (* Should Work *)

let d = 10 + "foo"
