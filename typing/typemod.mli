(* Type-checking of the module language *)

open Typedtree

val type_structure:
        Env.t -> Parsetree.structure -> structure * signature * Env.t
val transl_signature:
        Env.t -> Parsetree.signature -> signature

type error =
    Unbound_module of Longident.t
  | Unbound_modtype of Longident.t
  | Cannot_apply of module_type
  | Not_included of Includemod.error list
  | Cannot_eliminate_dependency of module_type
  | Signature_expected
  | Structure_expected of module_type
  | With_not_abstract of string
  | With_arity_mismatch of string

exception Error of Location.t * error

val report_error: error -> unit
