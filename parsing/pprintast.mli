(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

val structure: Format.formatter -> Parsetree.structure -> unit
val signature: Format.formatter -> Parsetree.signature -> unit
val expression: Format.formatter -> Parsetree.expression -> unit
val pattern: Format.formatter -> Parsetree.pattern -> unit
val core_type: Format.formatter -> Parsetree.core_type -> unit
val top_phrase: Format.formatter -> Parsetree.toplevel_phrase -> unit
