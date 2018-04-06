(* TEST
files = "a.ml b.ml c.ml main.ml main_ok.ml"
* setup-ocamlc.byte-build-env
** script
script = "mkdir -p subdir"
*** script
script = "cp ${test_source_directory}/subdir/m.ml subdir"
**** ocamlc.byte
module = "subdir/m.ml"
***** ocamlc.byte
flags = "-I subdir"
module = "a.ml"
****** ocamlc.byte
module = "b.ml"
******* ocamlc.byte
module = "c.ml"
******** ocamlc.byte
flags = ""
module = "main_ok.ml"
********* ocamlc.byte
module = "main.ml"
ocamlc_byte_exit_status = "2"
********** check-ocamlc.byte-output
*)
