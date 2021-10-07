(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Option values.

    Option values explicitly indicate the presence or absence of a value.

    @since 4.08 *)

(** {1:options Options} *)

type 'a t = 'a option = None | Some of 'a (**)
(** The type for option values. Either [None] or a value [Some v]. *)

val none : 'a option
(** [none] is [None]. *)

val some : 'a -> 'a option
(** [some v] is [Some v]. *)

val value : 'a option -> default:'a -> 'a
(** [value o ~default] is [v] if [o] is [Some v] and [default] otherwise. *)

val get : 'a option -> 'a
(** [get o] is [v] if [o] is [Some v] and raise otherwise.

    @raise Invalid_argument if [o] is [None]. *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
(** [bind o f] is [f v] if [o] is [Some v] and [None] if [o] is [None]. *)

val join : 'a option option -> 'a option
(** [join oo] is [Some v] if [oo] is [Some (Some v)] and [None] otherwise. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f o] is [None] if [o] is [None] and [Some (f v)] is [o] is [Some v]. *)

val fold : none:'a -> some:('b -> 'a) -> 'b option -> 'a
(** [fold ~none ~some o] is [none] if [o] is [None] and [some v] if [o] is
    [Some v]. *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f o] is [f v] if [o] is [Some v] and [()] otherwise. *)

(** {1:preds Predicates and comparisons} *)

val is_none : 'a option -> bool
(** [is_none o] is [true] if and only if [o] is [None]. *)

val is_some : 'a option -> bool
(** [is_some o] is [true] if and only if [o] is [Some o]. *)

val equal : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool
(** [equal eq o0 o1] is [true] if and only if [o0] and [o1] are both [None]
    or if they are [Some v0] and [Some v1] and [eq v0 v1] is [true]. *)

val compare : ('a -> 'a -> int) -> 'a option -> 'a option -> int
(** [compare cmp o0 o1] is a total order on options using [cmp] to compare
    values wrapped by [Some _]. [None] is smaller than [Some _] values. *)

(** {1:convert Converting} *)

val to_result : none:'e -> 'a option -> ('a, 'e) result
(** [to_result ~none o] is [Ok v] if [o] is [Some v] and [Error none]
    otherwise. *)

val to_list : 'a option -> 'a list
(** [to_list o] is [[]] if [o] is [None] and [[v]] if [o] is [Some v]. *)

val to_seq : 'a option -> 'a Seq.t
(** [to_seq o] is [o] as a sequence. [None] is the empty sequence and
    [Some v] is the singleton sequence containing [v]. *)
