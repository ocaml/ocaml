(* TEST
flags = "-no-alias-deps"
compile_only = "true"
files = "c.mli d.mli"
* setup-ocamlc.byte-build-env
** script
script = "cp ${test_source_directory}/b.cmi.invalid ${test_build_directory}/b.cmi"
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
