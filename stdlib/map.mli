(* Maps over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val iter: (key -> 'a -> 'b) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

module Make(Ord: OrderedType): (S with key = Ord.t)
