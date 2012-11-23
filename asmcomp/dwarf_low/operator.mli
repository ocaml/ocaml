type t

include Emittable.S with type t := t

val register : reg_number:int -> offset:int -> t
