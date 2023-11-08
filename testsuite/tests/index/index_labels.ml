(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
readonly_files = "index_labels.ml";
setup-ocamlc.byte-build-env;
all_modules = "index_labels.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index_labels.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
check-program-output;
*)

type t = { mutable a: int; b: string }

let x = { a = 42; b = "" }
let _y =
  x.a <- 32;
  x.a

let f = function
  | { a = 42; b } -> ()
  | _ -> ()
