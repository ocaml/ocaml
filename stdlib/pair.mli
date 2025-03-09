(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on pairs.

  @since 5.4 *)


(** {1:pairs Pairs} *)

type ('a, 'b) t = 'a * 'b
(** The type for pairs. *)

val make: 'a -> 'b -> 'a * 'b
(** [make a b] is the pair [(a, b)]. *)

val fst: 'a * 'b -> 'a
(** [fst (a, b)] is [a]. *)

val snd: 'a * 'b -> 'b
(** [snd (a, b)] is [b]. *)

val swap: 'a * 'b -> 'b * 'a
(** [swap (a, b)] is [(b, a)]. *)

(** {1:iters Iterators} *)

val fold: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [fold f (a, b)] applies [f] to [a] and [b]. *)

val map: ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd
(** [map f g (a, b)] applies [f] to [a] and [g] to [b]. *)

val iter: ('a -> unit) -> ('b -> unit) -> 'a * 'b -> unit
(** [iter f g (a, b)] first applies [f] to [a], and then [g] to [b]. *)

val map_fst: ('a -> 'c) -> 'a * 'b -> 'c * 'b
(** [map_fst f p] applies [f] to [p]'s first component. *)

val map_snd: ('b -> 'c) -> 'a * 'b -> 'a * 'c
(** [map_snd f p] applies [f] to [p]'s second component. *)

(** {1:preds Predicates and comparisons} *)

val equal:
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> 'a * 'b -> 'a * 'b -> bool
(** [equal eqa eqb (a1, b1) (a2, b2)] is [true] if and only if [eqa a1 a2] and
    [eqb b1 b2] are both [true]. *)

val compare:
  ('a -> 'a -> int) -> ('b -> 'b -> int) -> 'a * 'b -> 'a * 'b -> int
(** [compare cmpa cmpb] is a total order on pairs using [cmpa] to compare the
    first component, and [cmpb] to compare the second component. It is
    implemented by a lexicographic order. *)
