type t

include Emittable.S with type t := t

val compile_unit : t
val subprogram : t
val subprogram_with_no_children : t
val formal_parameter : t
val variable : t
val base_type : t

val child_determination : t -> Child_determination.t
