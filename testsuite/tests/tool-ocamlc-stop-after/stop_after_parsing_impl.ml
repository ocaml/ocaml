(* TEST_BELOW
Filler_text_added_to_pr
eserve_locatio
ns_while_translating_from_old_syntax__Filler_
text_added_to_preserve_locations_
while_translating_from_old_s
*)

(* we intentionally write ill-typed output;
   if `-stop-after parsing` was not supported properly,
   the test would fail with an error *)
let _ = (1 + "true") + x

(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-stop-after parsing -dparsetree";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
