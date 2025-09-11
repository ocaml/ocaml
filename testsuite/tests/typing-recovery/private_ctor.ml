(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = ..
type t += A
type t += private B


let x = (A, B)

let y = (A, A)

let z =
  let a = B in
  let b = A in
  (a, b)

let t : int = ""
