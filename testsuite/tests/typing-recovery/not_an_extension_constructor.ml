(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

type t = A | B | C
let x = [%extension_constructor A]
let f =
  let a = [%extension_constructor B]
  and b = [%extension_constructor C] in
  (a, b)

let u = (snd f, fst f, x)
let t : int = u
