(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-save-ir-after scheduling -stop-after scheduling"
   ocamlopt_byte_exit_status = "0"
 **** script
   script = "touch empty.ml"
 ***** ocamlopt.byte
   flags = "-S start_from_emit.cmir-linear"
   module = "empty.ml"
   ocamlopt_byte_exit_status = "0"
 ****** check-ocamlopt.byte-output
 ******* script
   script = "sh ${test_source_directory}/start_from_emit.sh"
 ******** ocamlopt.byte
   flags = "-S start_from_emit.cmir-linear -save-ir-after scheduling"
   module = "empty.ml"
   ocamlopt_byte_exit_status = "0"
 ********* copy
  src = "start_from_emit.cmir-linear"
  dst = "expected.cmir_linear"
 ********** check-ocamlopt.byte-output
 *********** script
   script = "cmp start_from_emit.cmir-linear expected.cmir_linear"

*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
