(* TEST
 readonly_files = "hello.c";
 setup-ocamlopt.opt-build-env;
 script = "mkdir outputdir";
 script;
 all_modules = "hello.c";
 compile_only = "true";
 flags = "-o outputdir/hello.${objext}";
 ocamlopt.opt;
 file = "outputdir/hello.${objext}";
 file-exists;
*)

(*
  This test makes sure it is possible to specify the name of the output
  object file when compiling a C file with the OCaml compiler.
  The test does not need to contain any OCaml code.
*)
