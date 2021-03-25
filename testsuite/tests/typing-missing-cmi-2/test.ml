(* TEST
readonly_files = "foo.mli bar.mli baz.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "foo.mli"
*** ocamlc.byte
module = "bar.mli"
**** script
script = "rm foo.cmi"
***** ocamlc.byte
flags = "-c -i"
module = "baz.ml"
ocamlc_byte_exit_status = "0"
****** check-ocamlc.byte-output
*)
