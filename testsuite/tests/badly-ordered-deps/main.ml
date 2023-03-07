(* TEST

(* Make sure ocamlc prints badly ordeered dependencies only once.
   See issue #12074. We test with ocamlc.byte only. *)

modules = "lib.ml"

* setup-ocamlc.byte-build-env

** ocamlc.byte
all_modules = "main.ml"
compile_only = "true"

*** ocamlc.byte
all_modules = "lib.ml"

**** ocamlc.byte
all_modules = "lib.cmo main.cmo"
compile_only = "false"
ocamlc_byte_exit_status = "2"

***** check-ocamlc.byte-output

*)

let value = ()
