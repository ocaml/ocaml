(* Association tables from any ordered type to any type.
   We use the generic ordering to compare keys. *)

type ('a, 'b) t

val empty: ('a, 'b) t
val add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val find: 'a -> ('a, 'b) t -> 'b

val iter: ('a -> 'b -> 'c) -> ('a, 'b) t -> unit

val print: ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
