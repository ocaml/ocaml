open Typedtree
open Lambda

type module_coercion = Types.module_coercion

(* XXX should also be done for toplevel ? *)

val transl_contracts: structure * module_coercion -> structure * module_coercion
