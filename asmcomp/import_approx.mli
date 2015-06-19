(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Import simple value approximations from .cmx files. *)

val really_import : Simple_value_approx.descr -> Simple_value_approx.descr
val really_import_approx : Simple_value_approx.t -> Simple_value_approx.t

val import_global : Ident.t -> Simple_value_approx.t
val import_symbol : Symbol.t -> Simple_value_approx.t
