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

let type_arrow ta tb =
  Ctype.newty (Tarrow (Asttypes.Nolabel, ta, tb, Types.commu_var ()))

let type_formatter () =
  let format = Path.Pident (Ident.create_persistent "Stdlib__Format") in
  Ctype.newconstr (Path.Pdot(format, "formatter")) []

let type_unit = Predef.type_unit

(*
  type 'a printer_type_old = 'a -> unit
  type 'a printer_type_new = Format.formatter -> 'a -> unit
*)
let printer_type_old alpha =
  type_arrow alpha type_unit

let printer_type_new alpha =
  type_arrow (type_formatter ()) (type_arrow alpha type_unit)
