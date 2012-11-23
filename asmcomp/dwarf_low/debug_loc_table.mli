type t

include Emittable.S with type t := t

val create : unit -> t

val insert : t
  -> location_list:Location_list.t
  -> t * Attribute_value.t
