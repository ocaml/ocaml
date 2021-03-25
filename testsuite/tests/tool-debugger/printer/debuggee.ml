(* TEST
flags += " -g "
ocamldebug_script = "${test_source_directory}/input_script"
readonly_files = "printer.ml"
include debugger
* debugger
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

let () = f 3
