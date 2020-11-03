(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-save-ir-after scheduling -S"
 **** check-ocamlopt.byte-output
 ***** script
   script = "sh ${test_source_directory}/save_ir_after_scheduling.sh"
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
