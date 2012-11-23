type t

include Emittable with type t := t

val in_register : reg_number:int -> t
