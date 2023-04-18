(* TEST_BELOW
Lorem_ipsum_dolor_
sit_amet,consectetur_adipiscing_
elit.Sed_non_risus.Lorem_ipsum_d
olor_sit_amet,con
sectetur_adipiscing_elit.Sed_no
*)

let int_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890z
let float_with_custom_modifier =
  1234567890_1234567890_1234567890_1234567890_1234567890.z

let int32     = 1234l
let int64     = 1234L
let nativeint = 1234n

let hex_without_modifier = 0x32f
let hex_with_modifier    = 0x32g

let float_without_modifer = 1.2e3
let float_with_modifer    = 1.2g

(* TEST
{
  flags = "-dparsetree";
  ocamlc_byte_exit_status = "2";
  setup-ocamlc.byte-build-env;

  ocamlc.byte;

  check-ocamlc.byte-output;
}
*)
