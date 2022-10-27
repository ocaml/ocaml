(* TEST
flags = "-no-alias-deps"
compile_only = "true"
readonly_files = "b.cmi.in c.mli d.mli"
* setup-ocamlc.byte-build-env
** copy
src = "b.cmi.in"
dst = "b.cmi"
*** ocamlc.byte
all_modules = "c.mli d.mli aliases.ml"
**** check-ocamlc.byte-output
***** ocamlobjinfo
program = "aliases.cmo"
****** check-program-output
*)

module A' = A (* missing a.cmi *)
module B' = B (* broken b.cmi *)
module C' = C (* valid c.cmi *)
module D' = D (* valid d.cmi *)
let () = print_int D'.something
