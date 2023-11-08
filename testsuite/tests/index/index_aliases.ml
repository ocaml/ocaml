(* TEST
flags = "-bin-annot -store-usage-index";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_aliases.ml";
ocamlc.byte;
check-ocamlc.byte-output;
program = "-index -decls index_aliases.cmt";
output = "out_objinfo";
ocamlobjinfo;
program = "awk '/Indexed/,0' out_objinfo";
output = "out_awk";
run;
check-program-output;
*)


module A = struct type t end
module B = A

module F (X : sig type t end) = X
module F' = F
module C = F'(A)

module C' = F(B)
module D = C
