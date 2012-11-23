type t

(* [emit] emits the attribute followed by the form. *)
include Emittable with type t := t

val low_pc : t
val high_pc : t
val producer : t
val name : t
val comp_dir : t
val stmt_list : t
val extern'l : t
val location : t
val typ' : t
val encoding : t
val byte_size : t
