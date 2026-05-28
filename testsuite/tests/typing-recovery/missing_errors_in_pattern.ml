(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type i = I of int

let f = function
  | I "foo" ->
      3.14 + 2

let f = function
  | J ->
      3.14 + 2

let f = function
  | `azdwbie
  | `c7diagq ->
      3.14 + 2
