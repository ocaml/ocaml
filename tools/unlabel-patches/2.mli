  module MakeSeeded (H : SeededHashedType) : SeededS
    with type key = H.t
     and type 'a t = 'a Hashtbl.MakeSeeded(H).t
