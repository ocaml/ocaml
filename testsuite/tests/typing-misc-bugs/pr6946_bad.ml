(* TEST_BELOW





*)

external foo : int = "%ignore";;
let _ = foo ();;

(* TEST
{
  flags = " -w -a ";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
