(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let _ =
  match (1, "hello") with
  | (x, _) | (_, x) -> x

let x = 10

let y = x ^ "ocaml"
