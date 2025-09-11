(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let _ = Format.asprintf "%d%\r" 10
let x = 10
let _ = Format.asprintf "%d%\e" 10
let _ : bool = 10
