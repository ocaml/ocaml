exception Not_constant

val run_ident_of_path : Path.t -> Rtype.run_ident
val tree_of_run_ident : Rtype.run_ident -> Outcometree.out_ident
val transl_run_ident_of_path : Path.t -> Lambda.structured_constant

val val_type_of_typexp : (Path.t -> 'a) -> Types.type_expr -> 'a Rtype.val_type

type digest = Abstract | Digest of string
val type_digest : Env.t -> Path.t -> digest

val run_type_of_typexp : Env.t -> Types.type_expr -> Rtype.run_type

val transl_run_type : Rtype.run_type -> Lambda.lambda
val tree_of_run_type : Rtype.run_type -> Outcometree.out_type

val transl_run_type_of_typexp : Env.t -> Types.type_expr -> Lambda.lambda

val rtype_prim : string -> Lambda.lambda

type error = Contains_abstract_type of Types.type_expr * Path.t
exception Error of error
