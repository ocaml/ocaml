type error =
    Unsupported
  | Cannot_have_full_path
  | Multiply_bound_type_variable

exception Error of Location.t * error

val get_rtype_type : unit -> Types.type_expr
val get_rtype_type_declaration : unit -> Types.type_expr

val pattern_of_type :
    bool (* permit non-linear type variable pattern or not *) -> 
      (Longident.t -> Path.t) -> Parsetree.core_type -> 
	Parsetree.pattern * string list list 

(* *
val value_of_type :
  (Longident.t -> Path.t) -> Parsetree.core_type -> Parsetree.expression
val to_core_types :
  (Types.type_expr * Types.type_expr) list ->
  Types.type_expr list ->
  Parsetree.core_type list * (Path.t * Longident.t) list * Ident.t list
* *)

val recover_type_expr :
  (Rtype.Path.t * 'a) Rtype.raw_type_expr -> Types.type_expr


(*
val recover_ident : string * int -> Ident.t
val recover_path : Rtype.Path.t -> Path.t
*)


(*
val reset_cached_runtime_type_tbl : unit -> unit
val runtime_type_expr : Types.type_expr -> Rtype.type_expr

val cached_runtime_type_tbl : (Types.type_expr * Rtype.type_expr) list ref
val dummy_runtime_type_declaration_tbl :
  (Rtype.type_declaration * Path.t) list ref
val reset_dummy_runtime_type_declaration_tbl : unit -> unit
val dummy_runtime_type_declaration : unit -> 'a Rtype.raw_type_declaration
val runtime_path : Path.t -> Rtype.Path.t
val runtime_type_desc :
  Types.type_desc ->
  (Rtype.Path.t * Rtype.type_declaration) Rtype.raw_type_desc
val runtime_private_flag : Asttypes.private_flag -> Rtype.private_flag
val runtime_record_representation :
  Types.record_representation -> Rtype.record_representation
val runtime_mutable_flag : Asttypes.mutable_flag -> Rtype.mutable_flag
val runtime_type_kind :
  Types.type_kind ->
  (Rtype.Path.t * Rtype.type_declaration) Rtype.raw_type_kind
*)
val runtime_type_exprs : 
    Types.type_expr list -> Rtype.type_expr list *
	(Types.type_expr * Rtype.type_expr) list * 
	(Rtype.type_declaration * Path.t) list

val runtime_type_declaration :
  Types.type_declaration ->
    Rtype.type_declaration * 
      (Types.type_expr * Rtype.type_expr) list * 
      (Rtype.type_declaration * Path.t) list

val report_error : Format.formatter -> error -> unit

