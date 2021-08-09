(* TEST

* setup-ocaml-build-env
** script
script = "mkdir -p test"
*** script
script = "cp ${test_source_directory}/driver.ml test/"
**** script
script = "cp ${test_source_directory}/payload.ml test/"
***** ocaml
test_file = "test/driver.ml"
ocaml_script_as_argument = "true"
*)
