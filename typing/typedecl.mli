(* Typing of type definitions *)

open Typedtree

val transl_type_decl:
        Env.t -> (string * Parsetree.type_declaration) list ->
                                  (Ident.t * type_declaration) list * Env.t
val transl_exception:
        Env.t -> Parsetree.exception_declaration -> exception_declaration

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string

exception Error of Location.t * error

val report_error: error -> unit
