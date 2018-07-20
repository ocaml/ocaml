module type Basic = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type S = sig
  include Applicative.S
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val compose_after : ('b -> 'c t) -> ('a -> 'b t) -> 'a -> 'c t
  val compose_before : ('a -> 'b t) -> ('b -> 'c t) -> 'a -> 'c t
end

module type Trans =
  functor (M : S) -> sig
    include Basic
    val lift : 'a M.t -> 'a t
  end

module Make (M : Basic) : S with type 'a t = 'a M.t = struct
  let bind = M.bind
  let (>>=) = M.bind
  let compose_after g f x = (f x) >>= g
  let compose_before f g x = (f x) >>= g

  module App =
    Applicative.Make(
        struct
          type 'a t = 'a M.t
          let (<*>) f x = bind f (fun f -> bind x (fun x -> M.return (f x)))
          let return = M.return
        end)

  include App
end
