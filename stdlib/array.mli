(* Array operations *)

val length : 'a array -> int = "%array_length"

val get: 'a array -> int -> 'a
val set: 'a array -> int -> 'a -> unit
val new: int -> 'a -> 'a array = "make_vect"
val new_matrix: int -> int -> 'a -> 'a array array
val concat: 'a array -> 'a array -> 'a array
val sub: 'a array -> int -> int -> 'a array
val copy: 'a array -> 'a array
val fill: 'a array -> int -> int -> 'a -> unit
val blit: 'a array -> int -> 'a array -> int -> int -> unit
val iter: ('a -> 'b) -> 'a array -> unit
val map: ('a -> 'b) -> 'a array -> 'b array
val to_list: 'a array -> 'a list
val of_list: 'a list -> 'a array

val unsafe_get: 'a array -> int -> 'a = "%array_get"
val unsafe_set: 'a array -> int -> 'a -> unit = "%array_set"

