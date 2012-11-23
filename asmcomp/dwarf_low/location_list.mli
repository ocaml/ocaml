type t

include Emittable with type t := t

val create : Location_list_entry.t list -> t
val label : t -> string
