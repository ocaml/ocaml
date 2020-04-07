(* TEST
files = "myppx.ml"
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
         -safe-string \
         -ppx ${program}"
**** ocamlc.byte
module = "test.ml"
flags = "-vmthread \
         -g \
         -no-alias-deps \
         -no-unboxed-types \
         -unsafe-string \
         -ppx ${program}"
***** check-ocamlc.byte-output
*)

(* empty *)
