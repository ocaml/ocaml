(* TEST
readonly_files = "error_location.mli error_location.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "error_location.mli"
*** ocamlc.byte
module = "error_location.ml"
ocamlc_byte_exit_status="2"
**** check-ocamlc.byte-output

*)

(* Test location in compilation unit error messages *)
