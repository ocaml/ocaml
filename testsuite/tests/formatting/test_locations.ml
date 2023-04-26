(* TEST_BELOW
Filler_text_a
dded_to_preserve_locations_wh
ile_translatin
g_from_old_syntax__Filler_text_added_to_preserve_locations_while_translating_from_o
ld_syntax__Filler_text_added
_to_preserve_locatio
ns_while_translating_from_old_syntax__Filler_text_added_to_preserve_locati

ons_while_translating_from_ol
d_syntax__Fill
er_text_added_to_preserve_locations_while_translating_from_old_syntax__Filler_te
xt_added_to_preserve_locatio
ns_while_translating
_from_old_syntax__Filler_text_added_to_preserve_locations_while_transla
*)
let rec fib = function
  | 0 | 1 -> 1
  | n -> fib (n - 1) + fib (n - 2)
;;

(* TEST
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
*)
