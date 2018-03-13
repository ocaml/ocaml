(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Seq]: functional iterators *)

(** {1 Functional Iterators} *)

(** The type ['a t] is a {b delayed list}, i.e. a list where some evaluation
    is needed to access the next element. This makes it possible to build
    infinite sequences, to build sequences as we traverse them, and to transform
    them in a lazy fashion rather than upfront.
*)

(** @since NEXT_RELEASE *)

(** The type of delayed lists containing elements of type ['a].
    Note that the concrete list node ['a node] is delayed under a closure,
    not a [lazy] block, which means it might be recomputed every time
    we access it. *)
type 'a t = unit -> 'a node

(** A fully-evaluated list node, either empty or containing an element
    and a delayed tail. *)
and +'a node =
  | Nil
  | Cons of 'a * 'a t

(** The empty sequence, containing no elements. *)
val empty : 'a t

(** The singleton sequence containing only the given element. *)
val return : 'a -> 'a t

(** [map f seq] returns a new sequence whose elements are the elements of
    [seq], transformed by [f].
    This transformation is lazy, it only applies when the result is traversed.

    If [seq = [1;2;3]], then [map f seq = [f 1; f 2; f 3]]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Remove from the sequence the elements that do not satisfy the
    given predicate.
    This transformation is lazy, it only applies when the result is traversed. *)
val filter : ('a -> bool) -> 'a t -> 'a t

(** Apply the function to every element; if [f x = None] then [x] is dropped;
    if [f x = Some y] then [y] is returned.
    This transformation is lazy, it only applies when the result is traversed. *)
val filter_map : ('a -> 'b option) -> 'a t -> 'b t

(** Map each element to a subsequence, then return each element of this
    sub-sequence in turn.
    This transformation is lazy, it only applies when the result is traversed. *)
val flat_map : ('a -> 'b t) -> 'a t -> 'b t

(** Traverse the sequence from left to right, combining each element with the
    accumulator using the given function.
    The traversal happens immediately and will not terminate on infinite sequences.

    Also see {!List.fold_left} *)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(** Iterate on the sequence, calling the (imperative) function on every element.
    The traversal happens immediately and will not terminate on infinite sequences. *)
val iter : ('a -> unit) -> 'a t -> unit
