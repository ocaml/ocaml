(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let a = 123455z
let b = 990098_09988777k

let u = match (a, b) with
  | 123455z -> "foo"
  |990098_09988777k -> "bar"

let r = u ^ "baz"
let k : string = 10
