(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x : int = "hello"

let f b =
  match b with
  | true -> 123
  | false -> "not a number"

let rec f x =
  1 + f [x]

let y = x + 10
