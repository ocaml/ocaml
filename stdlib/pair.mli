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

(** Function on pairs of values. *)


(** {1 Pairs} *)

type ('a, 'b) t = 'a * 'b
(** The type for pairs. *)

val make: 'a -> 'b -> ('a, 'b) t
(** [make a b] is the pair [(a, b)]. *)

val swap: ('a, 'b) t -> ('b, 'a) t
(** [swap (a, b)] is [(b, a)]. *)

(** {1 Iterators} *)

val fold: ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
(** [fold f (a, b)] applies [f] to [a] and [b]. *)

val uncurry: ('a -> 'b -> 'c) -> ('a, 'b) t -> 'c
(** [uncurry] is the same as {!fold}. *)

val map: ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t
(** [map f g (a, b)] applies [f] to [a] and [g] to [b]. *)

val iter: ('a -> unit) -> ('b -> unit) -> ('a, 'b) t -> unit
(** [iter f g (a, b)] first applies [f] to [a], and then [g] to [b]. *)

val map2:
  ('a -> 'c -> 'e) -> ('b -> 'd -> 'f) -> ('a, 'b) t -> ('c, 'd) t -> ('e, 'f) t
(** [map2 f g (a1, b1) (a2, b2)] applies [f] to [a1] and [a2], and [g] to [b1]
    and [b2]. *)

val iter2:
  ('a -> 'c -> unit) -> ('b -> 'd -> unit) -> ('a, 'b) t -> ('c, 'd) t -> unit
(** [iter2 f g (a1, b1) (a2, b2)] first applies [f] to [a1] and [a2], and then
    [g] to [b1] and [b2]. *)

val map_fst: ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
(** [map_fst f p] applies [f] to [p]'s first component. *)

val map_snd: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
(** [map_snd f p] applies [f] to [p]'s second component. *)

val iter_fst: ('a -> unit) -> ('a, 'b) t -> unit
(** [iter_fst f p] applies [f] to [p]'s first component. *)

val iter_snd: ('b -> unit) -> ('a, 'b) t -> unit
(** [iter_snd f p] applies [f] to [p]'s second component. *)

(** {1 Predicates and comparisons} *)

val equal: ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a, 'b) t -> ('a, 'b) t -> bool
(** [equal eqa eqb (a1, b1) (a2, b2)] is [true] if and only if [eqa a1 a2] and
    [eqb b1 b2] are both [true]. *)

val compare: ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
(** [compare cmpa cmpb] is a total order on pairs using [cmpa] to compare the
    first component, and [cmpb] to compare the second component. It is
    implemented by a lexicographic order. *)
