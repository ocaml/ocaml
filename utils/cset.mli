(* Sets over types ordered with the default ordering *)

type 'a t

val empty: 'a t
val mem: 'a -> 'a t -> bool
val add: 'a -> 'a t -> 'a t
val remove: 'a -> 'a t -> 'a t
val elements: 'a t -> 'a list
