(* TEST
 readonly_files = "largeFile.ml";
 setup-ocaml-build-env;
 compile_only = "true";
 all_modules = "largeFile.ml";
 ocamlc.byte;
 script = "mkdir -p inc";
 script;
 script = "mv largeFile.cmi largeFile.cmo inc/";
 script;
 ocaml;
 check-ocaml-output;
*)
#directory "inc";;
#load "largeFile.cmo";;
print_string LargeFile.message;;
