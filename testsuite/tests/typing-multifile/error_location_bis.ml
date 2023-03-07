(* TEST
readonly_files = "error_location_bis.mli error_location_bis.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "error_location_bis.mli"
*** ocamlc.byte
module = "error_location_bis.ml"
ocamlc_byte_exit_status="2"
**** check-ocamlc.byte-output

*)

(* Test location in compilation unit error messages *)
