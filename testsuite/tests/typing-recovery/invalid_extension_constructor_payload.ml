(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let x = [%extension_constructor]
let f =
  let a = [%extension_constructor]
  and b = [%extension_constructor] in
  (a, b)

let u = (snd f, fst f, x)
let t : int = u
