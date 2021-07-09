(* TEST

include dynlink
libraries = ""
subdirectories = "host plugin1 plugin2 plugin3 plugin4"

* shared-libraries
** setup-ocamlc.byte-build-env
*** cd
cwd = "plugin1"
*** ocamlc.byte
module = "api.mli"
*** ocamlc.byte
flags = "-for-pack Packed"
module = "api.ml"
*** ocamlc.byte
program = "packed.cmo"
flags = "-pack"
all_modules = "api.cmo"
*** ocamlc.byte
program = "plugin.cma"
flags = "-a"
all_modules = "plugin.ml"
*** cd
cwd = ".."

*** cd
cwd = "plugin2"
*** ocamlc.byte
module = "api.mli"
*** ocamlc.byte
flags = "-for-pack Packed"
module = "api.ml"
*** ocamlc.byte
program = "packed.cmo"
flags = "-pack"
all_modules = "api.cmo"
*** ocamlc.byte
program = "plugin.cma"
flags = "-a"
all_modules = "plugin.ml"
*** cd
cwd = ".."

*** cd
cwd = "plugin3"
*** ocamlc.byte
module = "api.mli"
*** ocamlc.byte
flags = "-for-pack Packed"
module = "api.ml"
*** ocamlc.byte
program = "packed.cmo"
flags = "-pack"
all_modules = "api.cmo"
*** ocamlc.byte
program = "plugin.cma"
flags = "-a"
all_modules = "packed.cmo plugin.ml"
*** cd
cwd = ".."

*** cd
cwd = "plugin4"
*** ocamlc.byte
module = "api.mli"
*** ocamlc.byte
flags = "-for-pack Packed"
module = "api.ml"
*** ocamlc.byte
program = "packed.cmo"
flags = "-pack"
all_modules = "api.cmo"
*** ocamlc.byte
program = "plugin.cma"
flags = "-a"
all_modules = "packed.cmo plugin.ml"
*** cd
cwd = ".."

*** cd
cwd = "host"
*** ocamlc.byte
module = "api.mli"
*** ocamlc.byte
flags = "-for-pack Packed"
module = "api.ml"
*** ocamlc.byte
program = "packed.cmo"
flags = "-pack"
all_modules = "api.cmo"
*** ocamlc.byte
program = "./host.byt"
libraries = "dynlink"
all_modules = "packed.cmo host.ml"
**** run
arguments = "../plugin1/plugin.cma"
output = "byte.plugin1.result"
***** check-program-output
reference = "${test_source_directory}/byte.plugin1.reference"
**** run
arguments = "../plugin2/plugin.cma"
output = "byte.plugin2.result"
***** check-program-output
reference = "${test_source_directory}/byte.plugin2.reference"
**** run
arguments = "../plugin3/plugin.cma"
output = "byte.plugin3.result"
***** check-program-output
reference = "${test_source_directory}/byte.plugin3.reference"
**** run
arguments = "../plugin4/plugin.cma"
output = "byte.plugin4.result"
***** check-program-output
reference = "${test_source_directory}/byte.plugin4.reference"
*** cd
cwd = ".."

** native-dynlink
*** setup-ocamlopt.byte-build-env

**** cd
cwd = "plugin1"
**** ocamlopt.byte
module = "api.mli"
**** ocamlopt.byte
flags = "-for-pack Packed"
module = "api.ml"
**** ocamlopt.byte
program = "packed.cmx"
flags = "-pack"
all_modules = "api.cmx"
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "plugin.ml"
**** cd
cwd = ".."

**** cd
cwd = "plugin2"
**** ocamlopt.byte
module = "api.mli"
**** ocamlopt.byte
flags = "-for-pack Packed"
module = "api.ml"
**** ocamlopt.byte
program = "packed.cmx"
flags = "-pack"
all_modules = "api.cmx"
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "plugin.ml"
*** cd
cwd = ".."

**** cd
cwd = "plugin3"
**** ocamlopt.byte
module = "api.mli"
**** ocamlopt.byte
flags = "-for-pack Packed"
module = "api.ml"
**** ocamlopt.byte
program = "packed.cmx"
flags = "-pack"
all_modules = "api.cmx"
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "packed.cmx plugin.ml"
**** cd
cwd = ".."

**** cd
cwd = "plugin4"
**** ocamlopt.byte
module = "api.mli"
**** ocamlopt.byte
flags = "-for-pack Packed"
module = "api.ml"
**** ocamlopt.byte
program = "packed.cmx"
flags = "-pack"
all_modules = "api.cmx"
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "packed.cmx plugin.ml"
**** cd
cwd = ".."

**** cd
cwd = "host"
**** ocamlopt.byte
module = "api.mli"
**** ocamlopt.byte
flags = "-for-pack Packed"
module = "api.ml"
**** ocamlopt.byte
program = "packed.cmx"
flags = "-pack"
all_modules = "api.cmx"
**** ocamlopt.byte
program = "./host.exe"
libraries = "dynlink"
all_modules = "packed.cmx host.ml"
***** run
arguments = "../plugin1/plugin.cmxs"
output = "native.plugin1.result"
****** check-program-output
reference = "${test_source_directory}/native.plugin1.reference"
***** run
arguments = "../plugin2/plugin.cmxs"
output = "native.plugin2.result"
****** check-program-output
reference = "${test_source_directory}/native.plugin2.reference"
***** run
arguments = "../plugin3/plugin.cmxs"
output = "native.plugin3.result"
****** check-program-output
reference = "${test_source_directory}/native.plugin3.reference"
***** run
arguments = "../plugin4/plugin.cmxs"
output = "native.plugin4.result"
****** check-program-output
reference = "${test_source_directory}/native.plugin4.reference"
**** cd
cwd = ".."
*)
