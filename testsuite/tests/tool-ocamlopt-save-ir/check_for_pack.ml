(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-save-ir-after scheduling";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "touch empty.ml";
 script;
 flags = "-S check_for_pack.cmir-linear -for-pack foo";
 module = "empty.ml";
 ocamlopt_byte_exit_status = "2";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
