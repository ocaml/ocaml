(* TEST

readonly_files = "A.ml B.ml C.ml D.ml lib_impl.ml lib.mli \
  Makefile.build Makefile.build2"

set sources = "A.ml B.ml C.ml D.ml"
set links = "LibA.ml LibB.ml LibC.ml LibD.ml"
set stdlib = "-nostdlib -I ${ocamlsrcdir}/stdlib"
set OCAMLC = "${ocamlrun} ${ocamlc_byte} ${stdlib}"
set OCAMLOPT = "${ocamlrun} ${ocamlopt_byte} ${stdlib}"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mk"
compiler_output = "${test_build_directory}/depend.mk"
** copy
src = "A.ml"
dst = "LibA.ml"
*** copy
src = "B.ml"
dst = "LibB.ml"
**** copy
src = "C.ml"
dst = "LibC.ml"
***** copy
src = "D.ml"
dst = "LibD.ml"
****** copy
src = "lib_impl.ml"
dst = "lib.ml"
******* ocamlc.byte
commandline = "-depend -as-map lib.ml lib.mli"
******** ocamlc.byte
commandline = "-depend -map lib.ml -open Lib ${links}"
********* check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mk.reference"
********** hasunix
*********** script
script = "rm -f ${links}"
************ script
script = "${MAKE} -f Makefile.build byte"
************* native-compiler
************** script
script = "${MAKE} -f Makefile.build opt"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mk2"
compiler_output = "${test_build_directory}/depend.mk2"
** copy
src = "A.ml"
dst = "LibA.ml"
*** copy
src = "B.ml"
dst = "LibB.ml"
**** copy
src = "C.ml"
dst = "LibC.ml"
***** copy
src = "D.ml"
dst = "LibD.ml"
****** ocamlc.byte
commandline = "-depend -map lib.mli -open Lib ${links}"
******* check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mk2.reference"
******** hasunix
********* script
script = "rm -f ${links}"
********** script
script = "${MAKE} -f Makefile.build2 byte"
*********** native-compiler
************ script
script = "${MAKE} -f Makefile.build2 opt"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod"
** copy
src = "A.ml"
dst = "LibA.ml"
*** copy
src = "B.ml"
dst = "LibB.ml"
**** copy
src = "C.ml"
dst = "LibC.ml"
***** copy
src = "D.ml"
dst = "LibD.ml"
****** copy
src = "lib_impl.ml"
dst = "lib.ml"
******* ocamlc.byte
commandline = "-depend -as-map -modules lib.ml lib.mli"
******** ocamlc.byte
commandline = "-depend -modules -map lib.ml -open Lib ${links}"
********* check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod.reference"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod2"
** copy
src = "A.ml"
dst = "LibA.ml"
*** copy
src = "B.ml"
dst = "LibB.ml"
**** copy
src = "C.ml"
dst = "LibC.ml"
***** copy
src = "D.ml"
dst = "LibD.ml"
****** ocamlc.byte
commandline = "-depend -modules -map lib.mli ${links}"
******* check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod2.reference"

* setup-ocamlc.byte-build-env
compiler_directory_suffix = ".depend.mod3"
** copy
src = "A.ml"
dst = "LibA.ml"
*** copy
src = "B.ml"
dst = "LibB.ml"
**** copy
src = "C.ml"
dst = "LibC.ml"
***** copy
src = "D.ml"
dst = "LibD.ml"
****** ocamlc.byte
commandline = "-depend -modules -as-map -map lib.mli -open Lib ${links}"
******* check-ocamlc.byte-output
compiler_reference = "${test_source_directory}/depend.mod3.reference"

*)

open Lib

let () = Printf.printf "B.g 3 = %d\n%!" (B.g 3)
