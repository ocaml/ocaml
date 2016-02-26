(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Gabriel Scherer, projet Gallium, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let p = print_endline

(* Test "_" bindings in `let rec` constructs. All bindings produce the same
 * effect to avoid testing evaluation order. *)
let test =
  let rec _ = p "1"; p "2"
      and x = p "1\n2"; 3
      and _ = p "1\n2"
      and y = 17
      and _ = ()
   in
   assert (x == 3); assert (y == 17);
   ()
