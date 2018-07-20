(** Module for the identity monad *)

type 'a t = 'a
(** Alias for the passed type *)

val map : ('a -> 'b) -> 'a -> 'b
(** Function application *)

val (<*>) : ('a -> 'b) -> 'a -> 'b
(** Function application *)

val bind : 'a -> ('a -> 'b) -> 'b
(** Reverse function application *)

val (>>=) : 'a -> ('a -> 'b) -> 'b
(** Infix reverse function application *)

val return : 'a -> 'a
(** The identity function *)
