(** The option type *)

module Trans (M : Monad.S) : sig
  type 'a t = 'a option M.t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val lift : 'a M.t -> 'a t
end

type 'a t = 'a option

val map : ('a -> 'b) -> 'a t -> 'b t

val (<*>) : ('a -> 'b) t -> 'a t -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t
