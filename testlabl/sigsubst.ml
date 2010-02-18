module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end
module type Comparable = sig
  type t
  val compare : t -> t -> int
end
module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end
module type PrintableComparable2 = sig
  include Printable
  include Comparable with type t := t
end

module type S = sig type t val f : t -> t end
module type S' = S with type t := int

module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type S' = S with type t := list
module type S' = sig
  type 'a dict = (string * 'a) list
  include S with type t := dict
end
