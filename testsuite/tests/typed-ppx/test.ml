(* TEST
files = "mytppx.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/mytppx.exe"
all_modules = "mytppx.ml"
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
         -tppx ${program}"
**** ocamlc.byte
module = "test.ml"
flags = "-vmthread \
         -g \
         -no-alias-deps \
         -no-unboxed-types \
         -unsafe-string \
         -tppx ${program}"
***** check-ocamlc.byte-output
*)

let () =
  let (a, b) = (6, 4) [@swap] in
    print_int a;
    print_newline ();
    print_int b;
    print_newline ()
