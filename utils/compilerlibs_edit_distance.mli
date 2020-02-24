
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
  cutoff:int ->
  test:('state -> 'a -> 'b -> ('c, 'd) result) ->
  update:(('a, 'b, 'c, 'd) change -> 'state -> 'state) ->
  'state -> 'a array -> 'b array -> ('a, 'b, 'c, 'd) patch option
