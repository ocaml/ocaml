
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

type ('left, 'right, 'eq, 'diff) change =
  | Delete of 'left
  | Insert of 'right
  | Keep of 'left * 'right * 'eq
  | Change of 'left * 'right * 'diff

val map :
  ('l1 -> 'l2) -> ('r1 -> 'r2) ->
  ('l1, 'r1, 'eq, 'diff) change ->
  ('l2, 'r2, 'eq, 'diff) change

type ('l, 'r, 'eq, 'diff) patch = ('l, 'r, 'eq, 'diff) change list

val diff :
  weight:(('l, 'r, 'eq, 'diff) change -> int) ->
  test:('state -> 'l -> 'r -> ('eq, 'diff) result) ->
  update:(('l, 'r, 'eq, 'diff) change -> 'state -> 'state) ->
  'state -> 'l array -> 'r array -> ('l, 'r, 'eq, 'diff) patch


(** Using the full state make it possible to resize the
    number of lines and columns dynamically.

    If only one side is ever expanded by the update function,
    the patch computation is guaranteed to terminate *)
type ('st,'line,'column) step =
  | No_expand of 'st
  | Expand_left of 'st * 'line array
  | Expand_right of 'st * 'column array

val dynamically_resized_diff :
  weight:(('l, 'r, 'eq, 'diff) change -> int) ->
  test:('state -> 'l -> 'r -> ('eq, 'diff) result) ->
  update:(('l, 'r, 'eq, 'diff) change -> 'state -> ('state, 'l, 'r) step) ->
  'state -> 'l array -> 'r array -> ('l, 'r, 'eq, 'diff) patch
