(* TEST_BELOW
Lorem_ipsum_dolor_sit_a
met,consectetu
r_adipiscing_elit.Sed_non_risus.Lorem_ipsum_d
olor_sit_amet,consectetur_adipisc
ing_elit.Sed_non_risus.Lorem
*)

(* we intentionally write ill-typed output;
   if `-stop-after parsing` was not supported properly,
   the test would fail with an error *)
val x : Module_that_does_not_exists.type_that_does_not_exists

(* TEST
{
  setup-ocamlc.byte-build-env;

  flags = "-stop-after parsing -dparsetree";
  ocamlc_byte_exit_status = "0";
  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
