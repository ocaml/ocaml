open Types
open Rtype

exception Not_constant

val run_ident_of_path : Path.t -> run_ident
val tree_of_run_ident : run_ident -> Outcometree.out_ident
val transl_run_ident_of_path : Path.t -> Lambda.structured_constant

type digest = Abstract | Digest of string
val type_digest : Env.t -> Path.t -> digest

val run_type_of_type_expr : Env.t -> type_expr -> run_type

val transl_run_type : run_type -> Lambda.lambda
val tree_of_run_type : run_type -> Outcometree.out_type

val transl_run_type_of_type_expr : Env.t -> type_expr -> Lambda.lambda

val rtype_prim : string -> Lambda.lambda

type error = Contains_abstract_type of type_expr * Path.t
exception Error of error

val path_of_run_ident : run_ident -> Path.t
val type_expr_of_run_type : Env.t -> run_type -> type_expr

