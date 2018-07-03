(* TEST

script = "sh ${test_source_directory}/check-linker-version.sh"
files = "mylib.mli mylib.ml stack_walker.c"

* macos
** script
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
flags = "-opaque"
module = "mylib.mli"
***** ocamlopt.byte
module = ""
flags = "-cclib -Wl,-keep_dwarf_unwind"
all_modules = "mylib.ml driver.ml stack_walker.c"
program = "${test_build_directory}/unwind_test"
****** run

*)

let () =
  Mylib.foo1 Mylib.bar 1 2 3 4 5 6 7 8 9 10;
  Mylib.foo2 Mylib.baz 1 2 3 4 5 6 7 8 9 10
