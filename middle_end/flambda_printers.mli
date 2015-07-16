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

val flambda : Format.formatter -> Flambda.t -> unit

val named : Format.formatter -> Flambda.named -> unit

val function_declaration
   : Format.formatter
  -> Variable.t * Flambda.function_declaration
  -> unit

val function_declarations
   : Format.formatter
  -> Flambda.function_declarations
  -> unit

val project_closure
   : Format.formatter
  -> Flambda.project_closure
  -> unit

val move_within_set_of_closures
   : Format.formatter
  -> Flambda.move_within_set_of_closures
  -> unit

val project_var
   : Format.formatter
  -> Flambda.project_var
  -> unit

val set_of_closures
   : Format.formatter
  -> Flambda.set_of_closures
  -> unit
