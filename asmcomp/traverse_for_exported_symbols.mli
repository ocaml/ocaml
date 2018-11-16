(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Fu Yong Quah, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type symbols_to_export =
  { symbols                               : Symbol.Set.t;
    export_ids                            : Export_id.Set.t;
    set_of_closure_ids                    : Set_of_closures_id.Set.t;
    set_of_closure_ids_keep_declaration   : Set_of_closures_id.Set.t;
    relevant_imported_closure_ids         : Closure_id.Set.t;
    relevant_local_closure_ids            : Closure_id.Set.t;
    relevant_imported_vars_within_closure : Var_within_closure.Set.t;
    relevant_local_vars_within_closure    : Var_within_closure.Set.t;
  }

(** Computes the transitive closure in [Symbol.t], [Closure_id.t] and
    [Set_of_closures_id.t] and determines which ones of those should be
    exported (i.e: included in the cmx files).
**)
val traverse
   : sets_of_closures_map: Flambda.set_of_closures Set_of_closures_id.Map.t
  -> closure_id_to_set_of_closures_id:
        Set_of_closures_id.t Closure_id.Map.t
  -> function_declarations_map:
        Simple_value_approx.function_declarations Set_of_closures_id.Map.t
  -> values: Export_info.descr Export_id.Map.t
  -> symbol_id: Export_id.t Symbol.Map.t
  -> root_symbol: Symbol.t
  -> symbols_to_export
