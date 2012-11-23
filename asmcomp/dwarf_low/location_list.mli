type t

include Emittable.S with type t := t

val create : Location_list_entry.t list -> t
val label : t -> string
