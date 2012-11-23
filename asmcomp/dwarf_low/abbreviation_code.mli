type t

include Emittable.S with type t := t

val of_int : int -> t
val null : unit -> t
