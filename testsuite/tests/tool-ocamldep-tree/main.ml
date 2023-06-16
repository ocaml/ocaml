(* TEST
 modules = "lib.ml main.ml";
 setup-ocamlc.byte-build-env;
 commandline = "-depend -all -source-tree $(srcdir) -build-tree $(OD) lib.ml main.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
 *)

let () = print_endline Lib.message
