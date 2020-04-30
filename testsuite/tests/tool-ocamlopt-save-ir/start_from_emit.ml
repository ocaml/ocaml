(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-save-ir-after scheduling -stop-after scheduling"
   ocamlopt_byte_exit_status = "0"
 **** script
   script = "cp start_from_emit.cmir-linear start_from_emit_cmir_linear.ml"
 ***** ocamlopt.byte
   flags = "-start-from emit -S start_from_emit.cmir-linear"
   module = "start_from_emit_cmir_linear.ml"
   ocamlopt_byte_exit_status = "0"
 ****** check-ocamlopt.byte-output
 ******* script
   script = "sh ${test_source_directory}/start_from_emit.sh"
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
