type 'a t

external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
external set : 'a t -> 'a -> unit = "%atomic_store"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
