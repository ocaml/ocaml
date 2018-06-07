(* TEST
files = "test_functor.ml test_loc_modtype_type_eq.ml \
         test_loc_modtype_type_subst.ml test_loc_type_eq.ml \
         test_loc_type_subst.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "test_functor.ml"
** ocamlc.byte
module = "test_loc_type_eq.ml"
ocamlc_byte_exit_status = "2"
** ocamlc.byte
module = "test_loc_modtype_type_eq.ml"
ocamlc_byte_exit_status = "2"
** ocamlc.byte
module = "test_loc_type_subst.ml"
ocamlc_byte_exit_status = "2"
** ocamlc.byte
module = "test_loc_modtype_type_subst.ml"
ocamlc_byte_exit_status = "2"
** check-ocamlc.byte-output
*)

