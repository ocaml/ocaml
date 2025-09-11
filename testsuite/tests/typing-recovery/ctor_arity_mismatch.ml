(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = A of int * string

let x = A 1

let f = match x with
  | A (y, _) -> y + 10

let y = x + "foo"
