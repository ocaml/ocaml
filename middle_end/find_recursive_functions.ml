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

let in_function_declarations (function_decls : Flambda.function_declarations)
      ~backend =
  let module VCC = Sort_connected_components.Make (Variable) in
  let function_variables = Variable.Map.keys function_decls.funs in
  (* This analysis may be run after lifting of constants; hence, references
     inside functions to others in the same set of closures may be through
     symbols.  We map these symbols back to variables before performing the
     SCC algorithm. *)
  let symbols_to_fun_vars =
    let module Backend = (val backend : Backend_intf.S) in
    Variable.Set.fold (fun fun_var symbols_to_fun_vars ->
        let closure_id = Closure_id.wrap fun_var in
        let symbol = Backend.closure_symbol closure_id in
        Symbol.Map.add symbol fun_var symbols_to_fun_vars)
      function_variables
      Symbol.Map.empty
  in
  let directed_graph =
    Variable.Map.map (fun (func_decl : Flambda.function_declaration) ->
        let from_symbols =
          Symbol.Set.fold (fun symbol fun_vars ->
              match Symbol.Map.find symbol symbols_to_fun_vars with
              | exception Not_found -> fun_vars
              | fun_var ->
                assert (Variable.Set.mem fun_var function_variables);
                Variable.Set.add fun_var fun_vars)
            func_decl.free_symbols
            Variable.Set.empty
        in
        let from_variables =
          Variable.Set.inter func_decl.free_variables function_variables
        in
        Variable.Set.union from_symbols from_variables)
      function_decls.funs
  in
  let connected_components =
    VCC.connected_components_sorted_from_roots_to_leaf directed_graph
  in
  Array.fold_left (fun rec_fun -> function
      | VCC.No_loop _ -> rec_fun
      | VCC.Has_loop elts -> List.fold_right Variable.Set.add elts rec_fun)
    Variable.Set.empty connected_components
