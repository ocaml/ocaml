(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License.         *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map. 
*)

module type OrderedType = sig type t val compare : t -> t -> int end
(** The input signature of the functor [Map.Make].
   [t] is the type of the map keys.
   [compare] is a total ordering function over the keys.
   This is a two-argument function [f] such that
   [f e1 e2] is zero if the keys [e1] and [e2] are equal,
   [f e1 e2] is strictly negative if [e1] is smaller than [e2],
   and [f e1 e2] is strictly positive if [e1] is greater than [e2].
   Example: a suitable ordering function is
   the generic structural comparison function {!Pervasives.compare}. *)


module type S =
  sig
    type key
    type +'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val find : key -> 'a t -> 'a
    val remove : key -> 'a t -> 'a t
    val mem : key -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

module Make (Ord : OrderedType) : S with type key = Ord.t
(** Functor building an implementation of the map structure
   given a totally ordered type. *)

