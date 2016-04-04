(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

val introduce_specialised_args_for_free_vars
   : backend:(module Backend_intf.S)
  -> Flambda.set_of_closures
  -> Flambda.set_of_closures

val replace_free_vars_by_equal_specialised_args
   : Flambda.set_of_closures
  -> Flambda.set_of_closures

val rewrite_function_declaration
   : free_vars:Variable.Map.key Variable.Map.t
  -> function_decl:Flambda.function_declaration
  -> specialised_args:Variable.Map.key Variable.Map.t
  -> Flambda.function_declaration
