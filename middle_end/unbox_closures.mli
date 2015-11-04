(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

val introduce_specialised_args_for_free_vars
   : Flambda.set_of_closures
  -> Flambda.set_of_closures

val replace_free_vars_by_equal_specialised_args
   : Flambda.set_of_closures
  -> Flambda.set_of_closures

val rewrite_function_declaration
   : free_vars:Variable.Map.key Variable.Map.t
  -> function_decl:Flambda.function_declaration
  -> specialised_args:Variable.Map.key Variable.Map.t
  -> Flambda.function_declaration
