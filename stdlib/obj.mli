(* Operations on internal representations of values *)

type t

external repr : 'a -> t = "%identity"
external magic : 'a -> 'b = "%identity"
external is_block : t -> bool = "obj_is_block"
external tag : t -> int = "%tagof"
external size : t -> int = "%array_length"
external field : t -> int -> t = "%array_get"
external set_field : t -> int -> t -> unit = "%array_set"
external new_block : int -> int -> t = "obj_block"
external update : t -> t -> unit = "%update"
