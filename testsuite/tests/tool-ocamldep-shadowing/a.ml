(* TEST

* setup-ocamlc.byte-build-env
** script
script = "cp -R ${test_source_directory}/dir1 ${test_source_directory}/dir2 ."
*** ocamlc.byte
commandline = "-depend -slash -I dir1 -I dir2 a.ml"
**** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/a.reference"
*)

include B
include C
