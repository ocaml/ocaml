(** Dynamic Arrays.

   This module implements amortized time complexity dynamic arrays.
 *)

type 'a dynArray
(** The type of dynamic arrays containing elements of type ['a]. *)

exception OutOfBounds
(** Raised when an index is out of the array. *)

val create : unit -> 'a dynArray
(** Returns a new empty dynamic array.*)

val make : int -> 'a -> 'a dynArray
(** [make n x] returns [n] element long dynamic array filled with [x].*)

val append : 'a dynArray -> 'a -> unit
(** [append a x] adds the element [x] at the end of dynamic array [a]*)

val get : 'a dynArray -> int -> 'a
(** [get a n] return the element with index [n] in dynamic array [a].*)

val set : 'a dynArray -> int -> 'a -> unit
(** [set a n x] sets the element at index [n] in dynamic array [a] to be [x].*)

val remove : 'a dynArray -> int -> unit
(** [remove a n] removes the element at position [n] in dynamic array [a].*)

val length : 'a dynArray -> int
(** [length a] returns the length of dynamic array [a].*)

val copy : 'a dynArray -> 'a dynArray
(** [copy a] returns a copy of dynamic array [a].*)

val clear : 'a dynArray -> unit
(** [clear a] removes all elements from dynamic array [a].*)

val insert : 'a dynArray -> int -> 'a -> unit
(** [insert a n x] inserts [x] in dynamic array [a] between element [n - 1] and [n].*)

val map : 'a dynArray -> ( 'a -> 'b ) -> 'b dynArray
(** [map a f] returns a dynamic array of [f] applied to all elements of dynamic array [a].*)

val iter : 'a dynArray -> ( 'a -> unit ) -> unit
(** [iter a f] applies f to each element of dynamic array [a].*)

val reverse : 'a dynArray -> unit
(** [reverse a] reverses dynamic array [a].*)

val switch : 'a dynArray -> int -> int -> unit
(** [switch a n m] switches elements at position [n] ans [m] in dynamic array [a].*)
