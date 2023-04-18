(* TEST_BELOW





*)

#1

(* TEST
{
  flags = "-dparsetree";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
