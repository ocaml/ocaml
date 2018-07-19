module type Basic = sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end
(** The minimum user-provided definition of a functor and input signature of the
    functor {!Functor.Make} *)

module type S = sig
  include Basic
end
(** The functor interface and output signature of the functor {!Functor.Make} *)

module Make (M : Basic) : S with type 'a t = 'a M.t
