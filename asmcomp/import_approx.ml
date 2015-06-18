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

open Abstract_identifiers

module A = Simple_value_approx

module SymbolTbl = Symbol.SymbolTbl
module SymbolMap = Symbol.SymbolMap

let reexported_missing_symbols = SymbolTbl.create 0

let rec import_ex ex =
  ignore (Compilenv.approx_for_global (Symbol.ExportId.unit ex));
  let ex_info = Compilenv.approx_env () in
  try match Flambdaexport.find_description ex ex_info with
    | Value_int i -> A.value_int i
    | Value_constptr i -> A.value_constptr i
    | Value_float f -> A.value_float f
    | Value_float_array size -> A.value_float_array size
    | Flambdaexport.Value_boxed_int (t,i) -> A.value_boxed_int t i
    | Value_string { size; contents } -> A.value_string size contents
    | Value_mutable_block _ -> A.value_unknown
    | Value_block (tag, fields) ->
      let tag = A.Tag.create_exn tag in
      A.value_block (tag, Array.map import_approx fields)
    | Value_closure { fun_id; closure = { closure_id; bound_var } } ->
      let bound_var = Var_within_closure.Map.map import_approx bound_var in
      let unchanging_params =
        try Set_of_closures_id.Map.find closure_id ex_info.ex_kept_arguments with
        | Not_found -> assert false
      in
      A.value_closure
        { closure_id = fun_id;
          set_of_closures_var = None;
          set_of_closures =
            { function_decls = Compilenv.imported_closure closure_id;
              bound_var;
              unchanging_params = unchanging_params;
              specialised_args = Variable.Set.empty;
              alpha_renaming =
                Flambdasubst.
                Alpha_renaming_map_for_ids_and_bound_vars_of_closures.empty;
            } }
    | Value_set_of_closures { closure_id; bound_var } ->
      let bound_var = Var_within_closure.Map.map import_approx bound_var in
      let unchanging_params =
        try Set_of_closures_id.Map.find closure_id ex_info.ex_kept_arguments with
        | Not_found -> assert false
      in
      A.value_set_of_closures
        { function_decls = Compilenv.imported_closure closure_id;
          bound_var;
          unchanging_params = unchanging_params;
          specialised_args = Variable.Set.empty;
          alpha_renaming =
            Flambdasubst.
            Alpha_renaming_map_for_ids_and_bound_vars_of_closures.empty; }
  with Not_found ->
    A.value_unknown

and import_approx (ap : Flambdaexport.approx) =
  match ap with
  | Value_unknown -> A.value_unknown
  | Value_id ex -> A.value_extern ex
  | Value_symbol sym -> A.value_symbol sym

let import_symbol sym =
  if Compilenv.is_predefined_exception sym then
    A.value_unknown
  else
    let symbol_id_map =
      (Compilenv.approx_for_global sym.sym_unit).ex_symbol_id in
    match import_ex (SymbolMap.find sym symbol_id_map) with
    | approx -> { approx with symbol = Some sym }
    | exception Not_found ->
      if not (SymbolTbl.mem reexported_missing_symbols sym)
      then begin
        SymbolTbl.add reexported_missing_symbols sym ();
        Location.prerr_warning (Location.in_file "some_file")
          (Warnings.Missing_symbol_information
             (Format.asprintf "%a" Symbol.print sym,
              Format.asprintf "%a" Symbol.Compilation_unit.print
                sym.Symbol.sym_unit));
      end;
      A.value_unresolved sym

let rec really_import (approx : A.descr) =
  match approx with
  | Value_extern ex -> really_import_ex ex
  | Value_symbol sym -> really_import_symbol sym
  | r -> r

and really_import_ex ex =
  really_import (import_ex ex).descr

and really_import_symbol sym =
  really_import (import_symbol sym).descr

let import_global id =
  let unit = Compilenv.unit_for_global id in
  import_approx
    (Ident.Map.find id (Compilenv.approx_for_global unit).ex_globals)

let really_import_approx (approx : Simple_value_approx.t) =
  { approx with descr = really_import approx.descr }
