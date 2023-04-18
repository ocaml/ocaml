(* TEST_BELOW
Lorem_ipsum_dolor_
sit_amet,consectetur_adipiscing_
elit.Sed_non_risus.Lorem_ipsum_d
olor_sit_amet,con
sectetur_adipiscing_elit.Sed_no
*)

let%foo x = 42
let%foo _ = () and _ = ()
let%foo _ = ()

(* TEST
{
  flags = "-dparsetree";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
