module type Printable = sig
  type t
  val print : Format.formatter -> t -> unit
end
[%%expect {|
module type Printable =
  sig type t val print : Format.formatter -> t -> unit end
|}]
module type Comparable = sig
  type t
  val compare : t -> t -> int
end
[%%expect {|
module type Comparable = sig type t val compare : t -> t -> int end
|}]
module type PrintableComparable = sig
  include Printable
  include Comparable with type t = t
end
[%%expect {|
Line _, characters 2-36:
Error: Multiple definition of the type name t.
       Names must be unique in a given structure or signature.
|}]

module type PrintableComparable = sig
  type t
  include Printable with type t := t
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig
    type t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
|}]
module type PrintableComparable = sig
  include Printable
  include Comparable with type t := t
end
[%%expect {|
module type PrintableComparable =
  sig
    type t
    val print : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end
|}]
module type ComparableInt = Comparable with type t := int
[%%expect {|
module type ComparableInt = sig val compare : int -> int -> int end
|}]
module type S = sig type t val f : t -> t end
[%%expect {|
module type S = sig type t val f : t -> t end
|}]
module type S' = S with type t := int
[%%expect {|
module type S' = sig val f : int -> int end
|}]

module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type S1 = S with type 'a t := 'a list
[%%expect {|
module type S = sig type 'a t val map : ('a -> 'b) -> 'a t -> 'b t end
module type S1 = sig val map : ('a -> 'b) -> 'a list -> 'b list end
|}]
module type S2 = sig
  type 'a dict = (string * 'a) list
  include S with type 'a t := 'a dict
end
[%%expect {|
module type S2 =
  sig
    type 'a dict = (string * 'a) list
    val map : ('a -> 'b) -> 'a dict -> 'b dict
  end
|}]


module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end
module M = struct type exp = string type arg = int end
module type S' = S with module T := M
[%%expect {|
module type S =
  sig module T : sig type exp type arg end val f : T.exp -> T.arg end
module M : sig type exp = string type arg = int end
module type S' = sig val f : M.exp -> M.arg end
|}]


module type S = sig type 'a t end with type 'a t := unit
[%%expect {|
Line _, characters 39-56:
Error: Only type constructors with identical parameters can be substituted.
|}]
