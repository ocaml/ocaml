(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Sebastien Hinderer, Tarides, Paris                   *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Infrastructure to support user-defined printers in toplevels and debugger *)

type printer_type = Types.type_expr -> Types.type_expr

val type_arrow : Types.type_expr -> Types.type_expr -> Types.type_expr

val printer_type_new : printer_type
val printer_type_old : printer_type
