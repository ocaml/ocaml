open Abstract_identifiers

module IntMap = Ext_types.Int.Map

type t

val create : unit -> t

val approx : t -> Flambdaapprox.t
val set_approx : t -> Flambdaapprox.t -> t

val use_var : t -> Variable.t -> t
val set_used_variables : t -> Variable.Set.t -> t
val used_variables : t -> Variable.Set.t

val exit_scope : t -> Variable.t -> t

val use_staticfail : t -> Static_exception.t -> t
val used_staticfail : t -> Static_exception.Set.t

val exit_scope_catch : t -> Static_exception.t -> t

val map_benefit : t -> (Flambdacost.benefit -> Flambdacost.benefit) -> t
val benefit : t -> Flambdacost.benefit
val clear_benefit : t -> t

val set_inline_threshold : t -> Flambdacost.inline_threshold -> t
val inline_threshold : t -> Flambdacost.inline_threshold

val add_global : t -> field_index:int -> approx:Flambdaapprox.t -> t
val find_global : t -> field_index:int -> Flambdaapprox.t
