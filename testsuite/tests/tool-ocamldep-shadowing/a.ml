(* TEST

subdirectories = "dir1 dir2"

* setup-ocamlc.byte-build-env
** ocamlc.byte
commandline = "-depend -slash -I dir1 -I dir2 a.ml"
*** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/a.reference"
*)

include B
include C
