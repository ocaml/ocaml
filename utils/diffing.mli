
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


type ('a, 'b, 'c, 'd) change =
  | Delete of 'a
  | Insert of 'b
  | Keep of 'a * 'b * 'c
  | Change of 'a * 'b * 'd

val map :
  ('a -> 'b) -> ('c -> 'd) -> ('a, 'c, 'e, 'f) change -> ('b, 'd, 'e, 'f) change

type ('a, 'b, 'c, 'd) patch = ('a, 'b, 'c, 'd) change list

val diff :
  weight:(('a, 'b, 'c, 'd) change -> int) ->
  test:('state -> 'a -> 'b -> ('c, 'd) result) ->
  update:(('a, 'b, 'c, 'd) change -> 'state -> 'state) ->
  'state -> 'a array -> 'b array -> ('a, 'b, 'c, 'd) patch


(** Using the full state make it possible to resize the
    number of lines and columns dynamically.

    If only one side is ever expanded by the update function,
    the patch computation is guaranteed to terminate *)
type ('inner,'line,'col) full_state =
  {
    line: 'line array;
    col: 'col array;
    inner:'inner
  }

val dynamically_resized_diff :
  weight:(('a, 'b, 'c, 'd) change -> int) ->
  test:('state -> 'a -> 'b -> ('c, 'd) result) ->
  update:
    (('a, 'b, 'c, 'd) change -> (('state,'a,'b) full_state as 'fs) -> 'fs) ->
  'fs -> ('a, 'b, 'c, 'd) patch
