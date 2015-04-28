
val spawn : (unit -> unit) -> unit

val self : unit -> int

type mutex

val mutex : unit -> mutex

val lock : mutex -> unit

val unlock : mutex -> unit
