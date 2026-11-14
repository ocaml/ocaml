(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* The module-dependent functor type domain stores a [package_type] record,
   which has no slot to carry a [Ptyp_extension]. An extension on the domain
   cannot be represented, so it must be rejected rather than silently dropped.
   Plain attributes (see below) are fine: they attach to the package type. *)
type t = (module%foo M : S) -> int
