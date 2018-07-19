module type Basic = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

module type S = sig
  include Basic
end

module Make (M : Basic) : S with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t
  let map = M.map
end
