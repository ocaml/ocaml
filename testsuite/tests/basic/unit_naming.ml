(* TEST
   modules = "camlCase.ml"
   * setup-ocamlc.byte-build-env
   ** ocamlc.byte
   ocamlc_byte_exit_status = "2"
   *** check-ocamlc.byte-output
*)

print_int Camlcase.answer
