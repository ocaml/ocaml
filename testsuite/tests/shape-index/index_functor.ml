(* TEST

flags = "-bin-annot -bin-annot-occurrences";
compile_only = "true";
setup-ocamlc.byte-build-env;
all_modules = "index_functor.ml";
ocamlc.byte;
check-ocamlc.byte-output;

program = "-quiet -index -decls index_functor.cmt";
output = "out_objinfo";
ocamlobjinfo;

check-program-output;
*)


module F (X :sig end ) = struct module M = X end
module N = F(struct end)
module O = N.M
include O
include N
