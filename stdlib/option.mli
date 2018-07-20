(** The option type *)

module Trans (M : Monad.S) : sig
  type 'a t = 'a option M.t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val lift : 'a M.t -> 'a t
end

include Monad.S with type 'a t = 'a option
