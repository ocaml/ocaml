(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compiler_diagnostic.Dev
type t = id Log.t
val source: string optional_field
val parsetree: string optional_field
val typedtree: string optional_field
val shape: string optional_field
val instr: string optional_field
val raw_lambda: string optional_field
val lambda: string optional_field
val flambda: string list optional_field
val raw_flambda: string list optional_field
val clambda: string list optional_field
val raw_clambda: string list optional_field
val cmm: string list optional_field
val remove_free_vars_equal_to_args: string list optional_field
val unbox_free_vars_of_closures: string list optional_field
val unbox_closures:string list optional_field
val unbox_specialised_args:string list  optional_field
val mach: string list optional_field
val linear: string list optional_field
val cmm_invariant: string optional_field
