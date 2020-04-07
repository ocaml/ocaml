(* TEST

modules = "aliases.ml external_for_pack.ml external.ml submodule.ml test.ml \
           use_in_pack.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/test.byte"
** ocamlc.byte
module = "submodule.ml"
flags = "-no-alias-deps"
*** ocamlc.byte
module = "aliases.ml"
**** ocamlc.byte
module = "external.mli"
***** ocamlc.byte
module = "external.ml"
****** ocamlc.byte
module = "external_for_pack.mli"
******* ocamlc.byte
module = "external_for_pack.ml"
******** ocamlc.byte
module = "test.ml"
********* ocamlc.byte
module = ""
flags = "-a -no-alias-deps"
all_modules = "submodule.cmo aliases.cmo external.cmo external_for_pack.cmo"
program = "mylib.cma"
********** ocamlc.byte
flags = "-no-alias-deps -for-pack P"
module = "use_in_pack.ml"
*********** ocamlc.byte
module = ""
program = "p.cmo"
flags = "-no-alias-deps -pack"
all_modules = "use_in_pack.cmo"
************ ocamlc.byte
program = "${test_build_directory}/test.byte"
all_modules = "mylib.cma p.cmo test.cmo"
flags= "-no-alias-deps"
************* check-ocamlc.byte-output
************** run
*************** check-program-output

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/test.opt"
** ocamlopt.byte
module = "submodule.ml"
flags = "-no-alias-deps"
*** ocamlopt.byte
module = "aliases.ml"
**** ocamlopt.byte
module = "external.mli"
***** ocamlopt.byte
module = "external.ml"
****** ocamlopt.byte
module = "external_for_pack.mli"
******* ocamlopt.byte
module = "external_for_pack.ml"
******** ocamlopt.byte
module = "test.ml"
********* ocamlopt.byte
module = ""
flags = "-no-alias-deps -a"
all_modules = "submodule.cmx aliases.cmx external.cmx external_for_pack.cmx"
program = "mylib.cmxa"
********** ocamlopt.byte
flags = "-no-alias-deps -for-pack P"
module = "use_in_pack.ml"
*********** ocamlopt.byte
module = ""
program = "p.cmx"
flags = "-no-alias-deps -pack"
all_modules = "use_in_pack.cmx"
************ ocamlopt.byte
program = "${test_build_directory}/test.opt"
all_modules = "mylib.cmxa p.cmx test.cmx"
flags = "-no-alias-deps"
************* check-ocamlopt.byte-output
************** run
*************** check-program-output

*)

include Aliases.Submodule.M
let _, _ = External.frexp 3.
