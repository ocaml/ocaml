(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Gabriel Scherer, projet Gallium, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = { x : t; y : t }

let p = print_endline

let test =
  let rec x = p "x"; { x = (p "x_x"; x); y = (p "x_y"; y) }
      and y = p "y"; { x = (p "y_x"; x); y = (p "y_y"; y) }
   in
   assert (x.x == x); assert (x.y == y);
   assert (y.x == x); assert (y.y == y);
   ()
