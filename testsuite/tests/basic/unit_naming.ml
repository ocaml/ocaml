(* TEST_BELOW





*)

print_int Camlcase.answer

(* TEST
{
  modules = "camlCase.ml";
  setup-ocamlc.byte-build-env;

  ocamlc_byte_exit_status = "2";
  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
