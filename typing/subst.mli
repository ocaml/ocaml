(* Substitutions *)

open Typedtree

type t

val identity: t

val add_type: Ident.t -> Path.t -> t -> t
val add_module: Ident.t -> Path.t -> t -> t
val add_modtype: Ident.t -> module_type -> t -> t

val type_expr: t -> type_expr -> type_expr
val value_description: t -> value_description -> value_description
val type_declaration: t -> type_declaration -> type_declaration
val exception_declaration:
        t -> exception_declaration -> exception_declaration
val modtype: t -> module_type -> module_type
val signature: t -> signature -> signature
val modtype_declaration: t -> modtype_declaration -> modtype_declaration

