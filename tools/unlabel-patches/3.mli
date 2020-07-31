  module Make : functor (Ord : OrderedType) -> S
    with type key = Ord.t
     and type 'a t = 'a Map.Make(Ord).t
