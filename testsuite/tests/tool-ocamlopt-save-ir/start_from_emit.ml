(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-save-ir-after scheduling -stop-after scheduling";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 script = "touch empty.ml";
 script;
 flags = "-S start_from_emit.cmir-linear";
 module = "empty.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/start_from_emit.sh";
 script;
 flags = "-S start_from_emit.cmir-linear -save-ir-after scheduling";
 module = "empty.ml";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 src = "start_from_emit.cmir-linear";
 dst = "expected.cmir_linear";
 copy;
 check-ocamlopt.byte-output;
 script = "cmp start_from_emit.cmir-linear expected.cmir_linear";
 script;
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
