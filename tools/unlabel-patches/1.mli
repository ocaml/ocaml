  module Make : functor (H : HashedType) -> S
    with type key = H.t
     and type 'a t = 'a Hashtbl.Make(H).t
