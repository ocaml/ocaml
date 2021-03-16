(* TEST
* setup-ocamlc.byte-build-env
compiler_output = "compiler-output.raw"
** ocamlc.byte
all_modules = "test.ml"
flags = "-warn-error +A"
ocamlc_byte_exit_status = "2"
*** script
script = "sh ${test_source_directory}/check-error-cleanup.sh"
*)

(* Regression test for MPR#7918 *)
let f () =
  (* -warn-error +A will error with unused x below *)
  let x = 12 in
  1
