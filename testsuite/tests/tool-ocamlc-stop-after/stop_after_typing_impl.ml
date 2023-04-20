(* TEST_BELOW
Filler_text_added_to_pr
eserve_locatio
ns_while_translating_from_old_syntax__Filler_text_added_to_p
reserve_locations_while_translati
ng_from_old_syntax__Filler_t
*)

(* we intentionally write an output that is type-correct
   but will be rejected before bytecode compilation
   due to the incorrect type given to the %apply
   compiler primitive. *)
external apply: int -> int = "%apply"

(* TEST
 setup-ocamlc.byte-build-env;
 flags = "-stop-after typing -dno-unique-ids -dtypedtree";
 ocamlc_byte_exit_status = "0";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
