(* TEST

files = "A.ml B.ml C.ml D.ml lib_impl.ml lib.mli"

script = "sh ${test_source_directory}/setup-links.sh"
set sources = "A.ml B.ml C.ml D.ml"
set links = "LibA.ml LibB.ml LibC.ml LibD.ml"
set stdlib = "-nostdlib -I ${ocamlsrcdir}/stdlib"
set OCAMLC = "${ocamlrun} ${ocamlc_byte} ${stdlib}"
set OCAMLOPT = "${ocamlrun} ${ocamlopt_byte} ${stdlib}"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mk"
compiler_output = "${test_build_directory}/depend.mk"
** script
*** script
script = "cp lib_impl.ml lib.ml"
**** ocamlc.byte
commandline = "-depend -as-map lib.ml lib.mli"
***** ocamlc.byte
commandline = "-depend -map lib.ml -open Lib ${links}"
****** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mk.reference"
******* hasunix
******** script
script = "cp ${test_source_directory}/Makefile.build Makefile"
********* script
script = "rm -f ${links}"
********** script
script = "${MAKE} byte"
*********** native-compiler
************ script
script = "${MAKE} opt"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mk2"
compiler_output = "${test_build_directory}/depend.mk2"
** script
*** ocamlc.byte
commandline = "-depend -map lib.mli -open Lib ${links}"
**** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mk2.reference"
***** hasunix
****** script
script = "rm -f ${links}"
******* script
script = "cp ${test_source_directory}/Makefile.build2 Makefile"
******** script
script = "${MAKE} byte"
********* native-compiler
********** script
script = "${MAKE} opt"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod"
** script
*** script
script = "cp lib_impl.ml lib.ml"
**** ocamlc.byte
commandline = "-depend -as-map -modules lib.ml lib.mli"
***** ocamlc.byte
commandline = "-depend -modules -map lib.ml -open Lib ${links}"
****** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod.reference"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod2"
** script
*** ocamlc.byte
commandline = "-depend -modules -map lib.mli ${links}"
**** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod2.reference"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod3"
** script
*** ocamlc.byte
commandline = "-depend -modules -as-map -map lib.mli -open Lib ${links}"
**** check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod3.reference"

*)

open Lib

let () = Printf.printf "B.g 3 = %d\n%!" (B.g 3)
