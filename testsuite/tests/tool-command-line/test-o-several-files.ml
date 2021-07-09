(* TEST
* setup-ocamlopt.opt-build-env
** ocamlopt.opt
all_modules = "foo.c bar.c"
compile_only = "true"
flags = "-o outputdir/baz.${objext}"
ocamlopt_opt_exit_status = "2"
*** check-ocamlopt.opt-output
*)

(*
  This test makes sure that the -o option is rejected when trying to
  compile several C files during the same invocatin of the OCaml compiler.
  The test does not need to contain any OCaml code.
*)
