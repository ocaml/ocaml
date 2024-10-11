  module Make : functor (Ord : OrderedType) -> S
    with type elt = Ord.t
     and type t = Set.Make(Ord).t
     and type Enum.enum = Set.Make(Ord).Enum.enum
