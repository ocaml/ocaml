(* TEST
 * native-compiler
 ** setup-ocamlopt.byte-build-env
 *** ocamlopt.byte
   flags = "-save-ir-after scheduling -stop-after scheduling"
   ocamlopt_byte_exit_status = "0"
 **** script
   script = "cp start_after_scheduling.cmir-linear start_after_scheduling_cmir_linear.ml"
 ***** ocamlopt.byte
   flags = "-start-after scheduling -S"
   module = "start_after_scheduling_cmir_linear.ml"
   ocamlopt_byte_exit_status = "0"
 ****** check-ocamlopt.byte-output
 ******* script
   script = "sh ${test_source_directory}/start_after_scheduling.sh"
*)

let foo f x =
  if x > 0 then x * 7 else f x

let bar x y = x + y
