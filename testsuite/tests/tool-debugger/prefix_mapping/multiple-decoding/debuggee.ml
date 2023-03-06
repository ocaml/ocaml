(* TEST
flags += " -g "
ocamldebug_script = "${test_source_directory}/input_script"
readonly_files = "printer.ml"
include debugger
* debugger
set BUILD_PATH_PREFIX_MAP = "/abc;/def=/workspace_root"
** shared-libraries
*** setup-ocamlc.byte-build-env
**** ocamlc.byte
module = "printer.ml"
**** ocamlc.byte
***** check-ocamlc.byte-output
****** ocamldebug
******* check-program-output
*)

let f x =
  for _i = 0 to x do
    print_endline "..."
  done

let main () =
  f 3
let () =
  main ()
