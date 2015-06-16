open Abstract_identifiers

module IntMap = Ext_types.Int.Map

type t

val create : unit -> t

val approx : t -> Simple_value_approx.t
val set_approx : t -> Simple_value_approx.t -> t

val use_var : t -> Variable.t -> t
val set_used_variables : t -> Variable.Set.t -> t
val used_variables : t -> Variable.Set.t

val exit_scope : t -> Variable.t -> t

val use_staticfail : t -> Static_exception.t -> t
val used_staticfail : t -> Static_exception.Set.t

val exit_scope_catch : t -> Static_exception.t -> t

val map_benefit : t -> (Inlining_cost.Benefit.t -> Inlining_cost.Benefit.t) -> t
val benefit : t -> Inlining_cost.Benefit.t
val clear_benefit : t -> t

val set_inlining_threshold : t -> Inlining_cost.inlining_threshold -> t
val inlining_threshold : t -> Inlining_cost.inlining_threshold

val add_global : t -> field_index:int -> approx:Simple_value_approx.t -> t
val find_global : t -> field_index:int -> Simple_value_approx.t
