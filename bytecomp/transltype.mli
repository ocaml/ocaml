exception Not_constant

val rpath_of_predefined_type : Ident.t -> Rtype.rpath
val rpath_of_path : Path.t -> Rtype.rpath
val transl_rpath : Rtype.rpath -> Lambda.structured_constant
val transl_rpath_of_ident : Lambda.lambda -> Ident.t -> Lambda.lambda
val transl_rpath_of_path : Path.t -> Lambda.structured_constant
val transl_rtype_of_type : Types.type_expr -> Lambda.lambda
val rtype_prim : string -> Lambda.lambda
