(* TEST
readonly_files = "hello.c"
* setup-ocamlopt.opt-build-env
** script
script = "mkdir outputdir"
*** ocamlopt.opt
all_modules = "hello.c"
compile_only = "true"
flags = "-o outputdir/hello.${objext}"
**** file-exists
file = "outputdir/hello.${objext}"
*)

(*
  This test makes sure it is possible to specify the name of the output
  object file when compiling a C file with the OCaml compiler.
  The test does not need to contain any OCaml code.
*)
