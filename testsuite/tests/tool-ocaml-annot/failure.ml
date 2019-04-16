(* TEST

flags = "-annot"
script = "sh ${test_source_directory}/check-annot.sh failure"
ocamlc_byte_exit_status = "2"
ocamlopt_byte_exit_status = "2"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** script

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** script
*)

(* Check that .annot files are emitted in case of failed compilation. *)
let a = 3
let b = a +. 1
