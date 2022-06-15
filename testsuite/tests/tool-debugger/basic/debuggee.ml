(* TEST
set foo = "bar"
flags += " -g "
ocamldebug_script = "${test_source_directory}/input_script"
* debugger
** shared-libraries
*** setup-ocamlc.byte-build-env
**** ocamlc.byte
***** check-ocamlc.byte-output
****** ocamldebug
******* check-program-output
*)

print_endline Sys.argv.(1);;
print_endline (Sys.getenv "foo");;
