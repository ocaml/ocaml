(* Inclusion checks for the core language *)

open Typedtree

val value_descriptions:
        Env.t -> value_description -> value_description -> bool
val type_declarations:
        Env.t -> Ident.t -> type_declaration -> type_declaration -> bool
val exception_declarations:
        Env.t -> exception_declaration -> exception_declaration -> bool
