type 'a t

external make : 'a -> 'a t = "%makemutable"
external get : 'a t -> 'a = "%atomic_load"
external exchange : 'a t -> 'a -> 'a = "%atomic_exchange"
external compare_and_set : 'a t -> 'a -> 'a -> bool = "%atomic_cas"
external fetch_and_add : int t -> int -> int = "%atomic_fetch_add"

let set r x =
  exchange r x |> ignore
let incr r =
  fetch_and_add r 1 |> ignore
let decr r =
  fetch_and_add r (-1) |> ignore
