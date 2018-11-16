(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)



val pretty_const : Asttypes.constant -> string
val top_pretty : Format.formatter -> Typedtree.pattern -> unit
val pretty_pat : Typedtree.pattern -> unit
val pretty_line : Format.formatter -> Typedtree.pattern list -> unit
val pretty_matrix : Format.formatter -> Typedtree.pattern list list -> unit
