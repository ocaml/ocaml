module type Basic = sig
  type 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end
(** The minimum user-provided definition of a monad and input signature of the
    functor {!Monad.Make} *)

module type S = sig
  include Applicative.S

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  (** Infix alias for [bind] *)
end
(** The monad interface and output signature of the functor {!Monad.Make} *)

module type Trans =
  functor (M : S) -> sig
    include Basic
    val lift : 'a M.t -> 'a t
  end
(** Monad transformer interface *)

module Make (M : Basic) : S with type 'a t = 'a M.t
