(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** References.

    References are mutable indirection cells containing a value of a given type.

    @since 4.12 *)

type 'a t = 'a ref = {mutable contents : 'a}
(** The type of references of type ['a]. *)

val ref : 'a -> 'a t
(** [ref x] creates a new reference with initial value [x]. *)

val get : 'a t -> 'a
(** [get r] returns the value stored in [r]. *)

val set : 'a t -> 'a -> unit
(** [set r x] sets the value stored in [r] to [x]. *)

val with_ref : 'a t -> 'a -> (unit -> 'b) -> 'b
(** [with_ref r x f] sets [r] to [x], and returns the result of [f], after
    restoring [r] to its previous value (even if an exception is raised by
    [f]. *)

val equal : 'a t -> 'a t -> bool
(** [equal r r'] is [r == r']. *)

val incr : int t -> unit
(** [incr r] is [set r (get r + 1)]. *)

val decr : int t -> unit
(** [decr r] is [set r (get r - 1)]. *)

val add_to_list : 'a list t -> 'a -> unit
(** [add_to_list r x] is [set r (x :: get r)]. *)
