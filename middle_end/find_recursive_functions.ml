(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let in_function_declarations (function_decls : Flambda.function_declarations)
      ~backend =
  let module VCC = Sort_connected_components.Make (Variable) in
  let directed_graph =
    Flambda_utils.fun_vars_referenced_in_decls function_decls ~backend
  in
  let connected_components =
    VCC.connected_components_sorted_from_roots_to_leaf directed_graph
  in
  Array.fold_left (fun rec_fun -> function
      | VCC.No_loop _ -> rec_fun
      | VCC.Has_loop elts -> List.fold_right Variable.Set.add elts rec_fun)
    Variable.Set.empty connected_components
