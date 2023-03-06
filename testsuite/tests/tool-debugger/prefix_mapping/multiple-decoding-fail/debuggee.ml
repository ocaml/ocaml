(* TEST
flags += " -g "
ocamldebug_script = "${test_source_directory}/input_script"
readonly_files = "printer.ml"
include debugger
* debugger
set BUILD_PATH_PREFIX_MAP = "/ocaml_root=${bppm_encode ${ocamlsrcdir}}"
BUILD_PATH_PREFIX_MAP += ":/current_dir=${bppm_encode .}"
BUILD_PATH_PREFIX_MAP += ":/stdlib_root=${bppm_encode ${ocamlsrcdir}}/stdlib"
BUILD_PATH_PREFIX_MAP += ":/source_root=${bppm_encode ${test_source_directory}}"
BUILD_PATH_PREFIX_MAP += ":/build_root=${bppm_encode ${test_build_directory}}"
** shared-libraries
*** setup-ocamlc.byte-build-env
**** dumpenv_expanded
***** ocamlc.byte
module = "printer.ml"
****** ocamlc.byte
unset module
******* check-ocamlc.byte-output
******** ocamldebug
BUILD_PATH_PREFIX_MAP = "/abc;/def=/workspace_root"
********* check-program-output
*)

let f x =
  for _i = 0 to x do
    print_endline "..."
  done

let main () =
  f 3
let () =
  main ()
