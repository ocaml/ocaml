module type Basic = sig
  type 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val return : 'a -> 'a t
end
(** The minimum user-provided definition of an applicative and input signature
    of the functor {!Applicative.Make} *)

module type S = sig
  include Functor.S
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
  val return : 'a -> 'a t
end
(** The applicative interface and output signature of the functor
    {!Applicative.Make} *)

module Make (M : Basic) : S with type 'a t = 'a M.t
