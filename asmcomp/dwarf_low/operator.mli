type t

include Emittable with type t := t

val register : reg_number:int -> offset:int -> t
