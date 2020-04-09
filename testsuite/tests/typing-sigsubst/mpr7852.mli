module M : sig
  type t
  val foo : t -> int
  val bar : t -> int
end

module N : sig
  type outer
  type t
  val foo : t -> outer
  val bar : t -> outer
end with type outer := int
