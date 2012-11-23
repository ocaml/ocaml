type t

include Emittable.S with type t := t

val in_register : reg_number:int -> t
