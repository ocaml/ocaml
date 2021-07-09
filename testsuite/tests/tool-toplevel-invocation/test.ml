(* TEST

readonly_files = "first_arg_fail.txt last_arg_fail.txt"

* setup-ocaml-build-env

** ocaml
flags = "-args ${test_source_directory}/first_arg_fail.txt"
compiler_reference = "${test_source_directory}/first_arg_fail.txt.reference"
compiler_output = "${test_build_directory}/first_arg_fail.output"
ocaml_exit_status = "2"
*** check-ocaml-output

** ocaml
flags = "-args ${test_source_directory}/indirect_first_arg_fail.txt"
compiler_reference =
  "${test_source_directory}/indirect_first_arg_fail.txt.reference"
compiler_output = "${test_build_directory}/indirect_first_arg_fail.output"
ocaml_exit_status = "2"
*** check-ocaml-output

** ocaml
flags = "-args ${test_source_directory}/indirect_last_arg_fail.txt"
compiler_reference =
  "${test_source_directory}/indirect_last_arg_fail.txt.reference"
compiler_output = "${test_build_directory}/indirect_last_arg_fail.output"
ocaml_exit_status = "2"
*** check-ocaml-output

** ocaml
flags = "-args ${test_source_directory}/last_arg_fail.txt"
compiler_reference = "${test_source_directory}/last_arg_fail.txt.reference"
compiler_output = "${test_build_directory}/last_arg_fail.output"
ocaml_exit_status = "2"
*** check-ocaml-output

** ocaml
flags = "-args ${test_source_directory}/working_arg.txt"
compiler_reference = "${test_source_directory}/working_arg.txt.reference"
compiler_output = "${test_build_directory}/working_arg.output"
*** check-ocaml-output

** ocaml
flags = "${test_source_directory}/print_args.ml foo bar"
compiler_reference = "${test_source_directory}/print_args.reference"
compiler_output = "${test_build_directory}/print_args.output"
*** check-ocaml-output

*)

printf "Test succeeds\n";;
