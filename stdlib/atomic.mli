(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Stephen Dolan, University of Cambridge                     *)
(*             Gabriel Scherer, projet Partout, INRIA Paris-Saclay        *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module provides a purely sequential implementation of the
    concurrent atomic references provided by the Multicore OCaml
    standard library:

    https://github.com/ocaml-multicore/ocaml-multicore/blob/parallel_minor_gc/stdlib/atomic.mli

    This sequential implementation is provided in the interest of
    compatibility: when people will start writing code to run on
    Multicore, it would be nice if their use of Atomic was
    backward-compatible with older versions of OCaml without having to
    import additional compatibility layers.

    @since 4.12
*)

(** An atomic (mutable) reference to a value of type ['a]. *)
type !'a t

(** Create an atomic reference. *)
val make : 'a -> 'a t

(** Get the current value of the atomic reference. *)
val get : 'a t -> 'a

(** Set a new value for the atomic reference. *)
val set : 'a t -> 'a -> unit

(** Set a new value for the atomic reference, and return the current value. *)
val exchange : 'a t -> 'a -> 'a

(** [compare_and_set r seen v] sets the new value of [r] to [v] only
    if its current value is physically equal to [seen] -- the
    comparison and the set occur atomically. Returns [true] if the
    comparison succeeded (so the set happened) and [false]
    otherwise. *)
val compare_and_set : 'a t -> 'a -> 'a -> bool

(** [fetch_and_add r n] atomically increments the value of [r] by [n],
    and returns the current value (before the increment). *)
val fetch_and_add : int t -> int -> int

(** [incr r] atomically increments the value of [r] by [1]. *)
val incr : int t -> unit

(** [decr r] atomically decrements the value of [r] by [1]. *)
val decr : int t -> unit
