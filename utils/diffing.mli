
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Gabriel Radanne, projet Cambium, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
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

    The underlying algorithm is modified Wagner-Fischer algorithm
    (see <https://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm>).
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

type ('st,'line,'column) step =
  | No_expand of 'st
  | Expand_left of 'st * 'line array
  | Expand_right of 'st * 'column array

(** [variadic_diff ~weight ~test ~update state l r] behaves as [diff]
    with the following difference:
    - [update ch st] must now return a {!step} which indicate if 
      the lists should be expanded.

    {b Warning}: To ensure termination, it is primordial that only one
    side is expanded during a diffing.
*)
val variadic_diff :
  weight:(('l, 'r, 'eq, 'diff) change -> int) ->
  test:('state -> 'l -> 'r -> ('eq, 'diff) result) ->
  update:(('l, 'r, 'eq, 'diff) change -> 'state -> ('state, 'l, 'r) step) ->
  'state -> 'l array -> 'r array -> ('l, 'r, 'eq, 'diff) patch
