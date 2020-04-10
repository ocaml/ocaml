(* TEST

flags = "-annot"
script = "sh ${test_source_directory}/check-annot.sh success"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** script

* setup-ocamlopt.byte-build-env
** ocamlopt.byte
*** script
*)

(* Check that .annot files are emitted in case of regular successful
   compilation. *)
let a = 3
let b = float a
