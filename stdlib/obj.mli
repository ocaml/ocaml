(* Operations on internal representations of values *)

type t

val repr : 'a -> t = "%identity"
val magic : 'a -> 'b = "%identity"
val is_block : t -> bool = "obj_is_block"
val tag : t -> int = "%tagof"
val size : t -> int = "%array_length"
val field : t -> int -> t = "%array_get"
val set_field : t -> int -> t -> unit = "%array_set"
val new_block : int -> int -> t = "obj_block"
val update : t -> t -> unit = "%update"
