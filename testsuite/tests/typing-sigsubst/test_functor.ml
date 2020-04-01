module type S = sig
  type elt
  type t

  val create : elt -> t
end

module Apply (Arg : sig type t end) : S with type elt = Arg.t = struct
  type elt = Arg.t
  type t = elt list

  let create x = [ x ]
end
