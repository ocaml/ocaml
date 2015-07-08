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

let in_function_decls ({ funs } : Flambda.function_declarations) =
  let module VCC = Sort_connected_components.Make (Variable) in
  let function_variables = Variable.Map.keys funs in
  let directed_graph =
    Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
        Variable.Set.inter func_decl.free_variables function_variables)
      funs
  in
  let connected_components =
    VCC.connected_components_sorted_from_roots_to_leaf directed_graph
  in
  Array.fold_left (fun rec_fun -> function
      | VCC.No_loop _ -> rec_fun
      | VCC.Has_loop elts -> List.fold_right Variable.Set.add elts rec_fun)
    Variable.Set.empty connected_components
