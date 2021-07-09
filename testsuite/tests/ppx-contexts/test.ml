(* TEST
readonly_files = "myppx.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/myppx.exe"
all_modules = "myppx.ml"
*** ocamlc.byte
module = "test.ml"
flags = "-thread \
         -I ${test_build_directory} \
         -open List \
         -rectypes \
         -principal \
         -alias-deps \
         -unboxed-types \
         -ppx ${program}"
**** ocamlc.byte
module = "test.ml"
flags = "-g \
         -no-alias-deps \
         -no-unboxed-types \
         -ppx ${program}"
***** check-ocamlc.byte-output
*)

(* empty *)
