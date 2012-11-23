type t

val size : t -> int
val emit : t -> emitter:Emitter.t -> unit
