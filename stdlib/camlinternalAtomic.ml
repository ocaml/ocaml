(* CamlinternalAtomic is a dependency of Stdlib, so it is compiled with
   -nopervasives. *)
type 'a t

(* Atomic is a dependency of Stdlib, so it is compiled with
   -nopervasives. *)
external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"
external ignore : 'a -> unit = "%ignore"

let set r x = ignore (exchange r x)
let incr r = ignore (fetch_and_add r 1)
let decr r = ignore (fetch_and_add r (-1))