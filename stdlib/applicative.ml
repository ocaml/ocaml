module type Basic = sig
  type 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val return : 'a -> 'a t
end

module type S = sig
  include Functor.S
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val return : 'a -> 'a t
end

module Make (M : Basic) : S with type 'a t = 'a M.t = struct
  let (<*>) = M.(<*>)
  let return = M.return

  module Functor =
    Functor.Make(
        struct
          type 'a t = 'a M.t
          let map f x = (return f) <*> x
        end)

  include Functor
end
