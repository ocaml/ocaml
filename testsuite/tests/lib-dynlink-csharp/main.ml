(* TEST

include dynlink

readonly_files = "entry.c main.cs plugin.ml"

* csharp-compiler
** shared-libraries
set csharp_cmd = "${csc} ${csc_flags} /out:main.exe main.cs"

*** setup-ocamlc.byte-build-env
**** ocamlc.byte
module = "plugin.ml"
***** ocamlc.byte
module = ""
flags = "-output-obj"
program = "main.dll"
all_modules = "dynlink.cma main.ml entry.c"
****** script
script = "${csharp_cmd}"
******* run
program = "./main.exe"
******** check-program-output
reference = "${test_source_directory}/main.bytecode.reference"

*** setup-ocamlc.byte-build-env
compiler_directory_suffix = "-dll"
**** ocamlc.byte
module = "plugin.ml"
***** ocamlc.byte
module = ""
flags = "-output-obj"
program = "main_obj.${objext}"
all_modules = "dynlink.cma entry.c main.ml"
****** script
script = "${mkdll} -maindll -o main.dll main_obj.${objext} entry.${objext} \
                   ${ocamlsrcdir}/runtime/libcamlrun.${libext} ${bytecc_libs}"
******* script
script = "${csharp_cmd}"
******** run
program = "./main.exe"
********* check-program-output
reference = "${test_source_directory}/main.bytecode.reference"

*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "plugin.ml"
***** ocamlopt.byte
flags = "-output-obj"
program= "main.dll"
all_modules = "dynlink.cmxa entry.c main.ml"
****** script
script = "${csharp_cmd}"
******* run
program = "./main.exe"
******** check-program-output
reference = "${test_source_directory}/main.native.reference"

*** setup-ocamlopt.byte-build-env
compiler_directory_suffix = "-dll"
**** ocamlopt.byte
program = "plugin.cmxs"
flags = "-shared"
all_modules = "plugin.ml"
***** ocamlopt.byte
flags = "-output-obj"
program = "main_obj.${objext}"
all_modules = "dynlink.cmxa entry.c main.ml"
****** script
script = "${mkdll} -maindll -o main.dll main_obj.${objext} entry.${objext} \
                   ${ocamlsrcdir}/runtime/libasmrun.${libext} ${nativecc_libs}"
******* script
script = "${csharp_cmd}"
******** run
program = "./main.exe"
********* check-program-output
reference = "${test_source_directory}/main.native.reference"

*)

let load s =
  Printf.printf "Loading %s\n%!" s;
  try
    Dynlink.loadfile s
  with Dynlink.Error e ->
    print_endline (Dynlink.error_message e)

(* Callback must be linked to load Unix dynamically *)
let _ = Callback.register
let _ = Stdlib.Bigarray.float32

let () =
  ignore (Hashtbl.hash 42.0);
  print_endline "Main is running.";
  Dynlink.allow_unsafe_modules true;
  let plugin_name = Dynlink.adapt_filename "plugin.cmo" in
  load plugin_name;
  print_endline "OK."
