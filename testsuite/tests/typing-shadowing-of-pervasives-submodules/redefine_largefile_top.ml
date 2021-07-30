(* TEST
   readonly_files = "largeFile.ml"
   * setup-ocaml-build-env
   ** ocamlc.byte
   compile_only = "true"
   all_modules = "largeFile.ml"
   *** script
   script = "mkdir -p inc"
   **** script
   script = "mv largeFile.cmi largeFile.cmo inc/"
   ***** ocaml
   ****** check-ocaml-output
*)
#directory "inc";;
#load "largeFile.cmo";;
print_string LargeFile.message;;
