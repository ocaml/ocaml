module type Basic = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type S = sig
  include Applicative.S
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type Trans =
  functor (M : S) -> sig
    include Basic
    val lift : 'a M.t -> 'a t
  end

module Make (M : Basic) : S with type 'a t = 'a M.t = struct
  let bind = M.bind
  let (>>=) = M.bind
  let return = M.return
  let (<*>) f x = bind f (fun f -> bind x (fun x -> return (f x)))

  module App =
    Applicative.Make(
        struct
          type 'a t = 'a M.t
          let (<*>) = (<*>)
          let return = return
        end)

  include App
end
