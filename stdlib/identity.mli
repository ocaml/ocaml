(** Module for the identity monad *)

type 'a t = 'a
(** Alias for the passed type *)

val map : ('a -> 'b) -> 'a -> 'b
(** Function application *)

val (<@@>) : ('a -> 'b) -> 'a -> 'b
(** Infix function application *)

val (<*>) : ('a -> 'b) -> 'a -> 'b
(** Infix function application *)

val bind : 'a -> ('a -> 'b) -> 'b
(** Reverse function application *)

val (>>=) : 'a -> ('a -> 'b) -> 'b
(** Infix reverse function application *)

val return : 'a -> 'a
(** The identity function *)

val compose_after : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Function composition; [compose_after g f x] is [g (f x)] *)

val compose_before : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition; [compose_before f g x] is [g (f x)] *)
