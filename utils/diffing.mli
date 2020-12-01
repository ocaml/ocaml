
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, Inria Paris               *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** {0 Parametric diffing}

    This module implements diffing over lists of arbitrary content.
    It is parameterized by
    - The content of the two lists
    - The equality witness when an element is kept
    - The diffing witness when an element is changed

    Diffing is extended to maintain state depending on the
    computed changes while walking through the two lists.

    The underlying algorithm is a modified Wagner-Fischer algorithm
    (see <https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm>).

    We provide the following guarantee:
    Given two lists [l] and [r], if different patches result in different
    states, we say that the state diverges.
    - We always return the optimal patch on prefixes of [l] and [r]
      on which state does not diverge.
    - Otherwise, we return a correct but non-optimal patch where subpatches
      with no divergent states are optimal for the given initial state.

    More precisely, the optimality of Wagner-Fischer depends on the property
    that the edit-distance between a k-prefix of the left input and a l-prefix
    of the right input d(k,l) satisfies

    d(k,l) = min (
     del_cost + d(k-1,l),
     insert_cost + d(k,l-1),
     change_cost + d(k-1,l-1)
    )

   Under this hypothesis, it is optimal to choose greedily the state of the
   minimal patch transforming the left k-prefix into the right l-prefix as a
   representative of the states of all possible patches transforming the left
   k-prefix into the right l-prefix.

   If this property is not satisfied, we can still choose greedily a
   representative state. However, the computed patch is no more guaranteed to
   be globally optimal.
   Nevertheless, it is still a correct patch, which is even optimal among all
   explored patches.

*)

(** The type of potential changes on a list. *)
type ('left, 'right, 'eq, 'diff) change =
  | Delete of 'left
  | Insert of 'right
  | Keep of 'left * 'right * 'eq
  | Change of 'left * 'right * 'diff

val map :
  ('l1 -> 'l2) -> ('r1 -> 'r2) ->
  ('l1, 'r1, 'eq, 'diff) change ->
  ('l2, 'r2, 'eq, 'diff) change

(** A patch is an ordered list of changes. *)
type ('l, 'r, 'eq, 'diff) patch = ('l, 'r, 'eq, 'diff) change list

(** [diff ~weight ~test ~update state l r] computes
    the diff between [l] and [r], using the initial state [state].
    - [test st xl xr] tests if the elements [xl] and [xr] are
      compatible ([Ok]) or not ([Error]).
    - [weight ch] returns the weight of the change [ch].
      Used to find the smallest patch.
    - [update ch st] returns the new state after applying a change.
*)
val diff :
  weight:(('l, 'r, 'eq, 'diff) change -> int) ->
  test:('state -> 'l -> 'r -> ('eq, 'diff) result) ->
  update:(('l, 'r, 'eq, 'diff) change -> 'state -> 'state) ->
  'state -> 'l array -> 'r array -> ('l, 'r, 'eq, 'diff) patch

(** {1 Variadic diffing}

    Variadic diffing allows to expand the lists being diffed during diffing.
*)

type ('l, 'r, 'e, 'd, 'state) update =
  | Without_extensions of (('l,'r,'e,'d) change -> 'state -> 'state)
  | With_left_extensions of
      (('l,'r,'e,'d) change -> 'state -> 'state * 'l array)
  | With_right_extensions of
      (('l,'r,'e,'d) change -> 'state -> 'state * 'r array)

(** [variadic_diff ~weight ~test ~update state l r] behaves as [diff]
    with the following difference:
    - [update] must now be an {!update} which indicates in which direction
      the expansion takes place.
*)
val variadic_diff :
  weight:(('l, 'r, 'eq, 'diff) change -> int) ->
  test:('state -> 'l -> 'r -> ('eq, 'diff) result) ->
  update:('l, 'r, 'eq, 'diff, 'state) update ->
  'state -> 'l array -> 'r array -> ('l, 'r, 'eq, 'diff) patch
