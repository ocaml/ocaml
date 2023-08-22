(* TEST
modules = "dep.ml a.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
commandline = "-depend -strict a.ml doesnotexist.ml"
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)
