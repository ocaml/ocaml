(* Sets over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val iter: (elt -> 'a) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val elements: t -> elt list
  end

module Make(Ord: OrderedType): (S with elt = Ord.t)
