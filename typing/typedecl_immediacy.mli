type error = Bad_immediate_attribute
exception Error of Location.t * error

val compute_decl : Env.t -> Types.type_declaration -> bool

val property : (bool, unit) Typedecl_properties.property

val update_decls :
  Env.t ->
  (Ident.t * Typedecl_properties.decl) list ->
  (Ident.t * Typedecl_properties.decl) list
