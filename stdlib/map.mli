(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Module [Map]: association tables over ordered types *)

(* This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end
          (* The input signature of the functor [Map.Make].
             [t] is the type of the map keys.
             [compare] is a total ordering function over the keys.
             This is a two-argument function [f] such that
             [f e1 e2] is zero if the keys [e1] and [e2] are equal,
             [f e1 e2] is strictly negative if [e1] is smaller than [e2],
             and [f e1 e2] is strictly positive if [e1] is greater than [e2].
             Example: a suitable ordering function is
             the generic structural comparison function [compare]. *)

module type S =
  sig
    type key
          (* The type of the map keys. *)
    type 'a t
          (* The type of maps from type [key] to type ['a]. *)
    val empty: 'a t
          (* The empty map. *)
    val add: key -> 'a -> 'a t -> 'a t
        (* [add x y m] returns a map containing the same bindings as
           [m], plus a binding of [x] to [y]. If [x] was already bound
           in [m], its previous binding disappears. *)
    val find: key -> 'a t -> 'a
        (* [find x m] returns the current binding of [x] in [m],
           or raises [Not_found] if no such binding exists. *)
    val remove: key -> 'a t -> 'a t
        (* [remove x m] returns a map containing the same bindings as
           [m], except for [x] which is unbound in the returned map. *)
    val mem:  key -> 'a t -> bool
        (* [mem x m] returns [true] if [m] contains a binding for [m],
           and [false] otherwise. *)
    val iter: (key -> 'a -> unit) -> 'a t -> unit
        (* [iter f m] applies [f] to all bindings in map [m].
           [f] receives the key as first argument, and the associated value
           as second argument. The order in which the bindings are passed to
           [f] is unspecified. Only current bindings are presented to [f]:
           bindings hidden by more recent bindings are not passed to [f]. *)
    val map: ('a -> 'b) -> 'a t -> 'b t
        (* [map f m] returns a map with same domain as [m], where the
           associated value [a] of all bindings of [m] has been
           replaced by the result of the application of [f] to [a].
           The order in which the associated values are passed to [f]
           is unspecified. *)
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        (* [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
           where [k1 ... kN] are the keys of all bindings in [m],
           and [d1 ... dN] are the associated data.
           The order in which the bindings are presented to [f] is
           unspecified. *)
  end

module Make(Ord: OrderedType): (S with type key = Ord.t)
        (* Functor building an implementation of the map structure
           given a totally ordered type. *)
