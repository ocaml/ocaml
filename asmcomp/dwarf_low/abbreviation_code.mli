type t

include Emittable with type t := t

val of_int : int -> t
val null : unit -> t
