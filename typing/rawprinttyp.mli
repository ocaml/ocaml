(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Jacques Garrigue, Graduate School of Mathematics, Nagoya University   *)
(*                                                                        *)
(*   Copyright 2003 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** This module provides function(s) for printing the internal representation of
    type expressions. It is targetted at internal use when debbuging the
    compiler itself. *)

val type_expr: Format.formatter -> Types.type_expr -> unit
