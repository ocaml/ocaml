(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f (x : int) : int = x + 1

let _ = f "hello"


let _ =
  (1 : string)

let g (x : string) = x ^ "!"

let _ =
  let y = g 123 in
  y
