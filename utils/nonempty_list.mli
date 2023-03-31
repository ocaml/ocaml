
type 'a t = (::) of 'a * 'a list

val length : _ t -> int

val compare_lengths : 'a t -> 'a t -> int

val cons : 'a -> 'a t -> 'a t

val hd : 'a t -> 'a
val tl : 'a t -> 'a list

val to_list : 'a t -> 'a list
val of_list_exn : 'a list -> 'a t
val of_array_exn : 'a array -> 'a t

(** {1 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f [a1; ...; an]] applies function [f] in turn to
   [[a1; ...; an]]. It is equivalent to
   [f a1; f a2; ...; f an].
 *)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Same as {!iter}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
 *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
   and builds the list [[f a1; ...; f an]]
   with the results returned by [f].
 *)

val map_to_list : ('a -> 'b) -> 'a t -> 'b list

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Same as {!map}, but the function is applied to the index of
   the element as first argument (counting from 0), and the element
   itself as second argument.
 *)

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
(** [fold_left f init [b1; ...; bn]] is
   [f (... (f (f init b1) b2) ...) bn].
 *)

val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
(** [fold_right f [a1; ...; an] init] is
   [f a1 (f a2 (... (f an init) ...))]. Not tail-recursive.
 *)

(** {1 List scanning} *)

val for_all : ('a -> bool) -> 'a t -> bool
(** [for_all f [a1; ...; an]] checks if all elements of the list
   satisfy the predicate [f]. That is, it returns
   [(f a1) && (f a2) && ... && (f an)] for a non-empty list and
   [true] if the list is empty.
 *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
(** Same as {!for_all}, but for a two-argument predicate.
   @raise Invalid_argument if the two lists are determined
   to have different lengths.
 *)

val exists : ('a -> bool) -> 'a t -> bool
(** [exists f [a1; ...; an]] checks if at least one element of
    the list satisfies the predicate [f].
 *)

(** {1 List searching} *)

val find : ('a -> bool) -> 'a t -> 'a
(** [find f l] returns the first element of the list [l]
   that satisfies the predicate [f].
   @raise Not_found if there is no value that satisfies [f] in the
   list [l].
 *)

val filter : ('a -> bool) -> 'a t -> 'a list
(** [filter f l] returns all the elements of the list [l]
   that satisfy the predicate [f]. The order of the elements
   in the input list is preserved.
 *)

(** {1 Association lists} *)

val assoc : 'a -> ('a * 'b) t -> 'b
(** [assoc a l] returns the value associated with key [a] in the list of
   pairs [l]. That is,
   [assoc a [ ...; (a,b); ...] = b]
   if [(a,b)] is the leftmost binding of [a] in list [l].
   @raise Not_found if there is no value associated with [a] in the
   list [l].
 *)

val mem_assoc : 'a -> ('a * 'b) t -> bool
(** Same as {!assoc}, but simply return [true] if a binding exists,
   and [false] if no bindings exist for the given key.
 *)
