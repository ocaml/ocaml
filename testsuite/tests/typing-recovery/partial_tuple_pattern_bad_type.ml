(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let f (z, (~y, ~x, ..)) = x, y, z
let g (~x, ~y, ..) = x, y

let x =
  let (a, b, c) = f (10, (~y:11, ~x:12)) in
  let (d, e) = g (~x:22, ~y:33) in
  a + b + c + d + e
