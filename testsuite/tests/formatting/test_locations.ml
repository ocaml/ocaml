(* TEST_BELOW
Lorem_ipsum_d
olor_sit_amet,consectetur_adi
piscing_elit.S
ed_non_risus.Lorem_ipsum_dolor_sit_amet,consectetur_adipiscing_elit.Sed_non_risus.L
orem_ipsum_dolor_sit_amet,co
nsectetur_adipiscing
_elit.Sed_non_risus.Lorem_ipsum_dolor_sit_amet,consectetur_adipiscing_elit

.Sed_non_risus.Lorem_ipsum_do
lor_sit_amet,c
onsectetur_adipiscing_elit.Sed_non_risus.Lorem_ipsum_dolor_sit_amet,consectetur_
adipiscing_elit.Sed_non_risu
s.Lorem_ipsum_dolor_
sit_amet,consectetur_adipiscing_elit.Sed_non_risus.Lorem_ipsum_dolor_si
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

(* TEST
{
  compile_only = "true";
  {
    setup-ocamlc.byte-build-env;

    flags = "-g -dno-unique-ids -dno-locations -dsource -dparsetree -dtypedtree -dlambda";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/test_locations.dno-locations.ocamlc.reference";
    check-ocamlc.byte-output;
  }{
    setup-ocamlc.byte-build-env;

    flags = "-g -dno-unique-ids -dlocations -dsource -dparsetree -dtypedtree -dlambda";
    ocamlc.byte;

    compiler_reference = "${test_source_directory}/test_locations.dlocations.ocamlc.reference";
    check-ocamlc.byte-output;
  }
}
*)
