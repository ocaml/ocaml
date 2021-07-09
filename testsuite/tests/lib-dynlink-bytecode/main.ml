(* TEST

include dynlink

ld_library_path += "${test_build_directory}"

readonly_files = "plug1.ml plug2.ml registry.ml stub1.c stub2.c"

* shared-libraries
** setup-ocamlc.byte-build-env
*** ocamlc.byte
compile_only = "true"
all_modules = "registry.ml stub1.c stub2.c plug1.ml plug2.ml main.ml"
**** ocamlmklib
program = "plug1"
modules = "stub1.${objext}"
***** ocamlmklib
program = "plug2"
modules = "stub2.${objext}"
****** ocamlmklib
program = "plug1"
modules = "plug1.cmo"
******* ocamlmklib
program = "plug2"
modules = "plug2.cmo"

compile_only = "false"

******** ocamlc.byte
program = "${test_build_directory}/main.exe"
all_modules = "registry.cmo main.cmo"
********* run
arguments = "plug1.cma plug2.cma"
output = "main.output"
********** check-program-output

******** ocamlc.byte
program = "${test_build_directory}/static.exe"
flags = "-linkall"
all_modules = "registry.cmo plug1.cma plug2.cma"
********* run
output = "static.output"
********** check-program-output
reference = "${test_source_directory}/static.reference"

******** ocamlc.byte
program = "${test_build_directory}/custom.exe"
flags = "-custom -linkall -I ."
all_modules = "registry.cmo plug2.cma plug1.cma"
use_runtime = "false"
********* run
output = "custom.output"
********** check-program-output
reference = "${test_source_directory}/custom.reference"

*)

let f x = print_string "This is Main.f\n"; x

let () = Registry.register f

let _ =
  Dynlink.allow_unsafe_modules true;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    Printf.printf "Loading %s\n" name; flush stdout;
    try
      if name.[0] = '-'
      then Dynlink.loadfile_private
        (String.sub name 1 (String.length name - 1))
      else Dynlink.loadfile name
    with
      | Dynlink.Error err ->
          Printf.printf "Dynlink error: %s\n"
            (Dynlink.error_message err)
      | exn ->
          Printf.printf "Error: %s\n" (Printexc.to_string exn)
  done;
  flush stdout;
  try
    let oc = open_out_bin "marshal.data" in
    Marshal.to_channel oc (Registry.get_functions()) [Marshal.Closures];
    close_out oc;
    let ic = open_in_bin "marshal.data" in
    let l = (Marshal.from_channel ic : (int -> int) list) in
    close_in ic;
    List.iter
      (fun f ->
        let res = f 0 in
        Printf.printf "Result is: %d\n" res)
      l
  with Failure s ->
    Printf.printf "Failure: %s\n" s
