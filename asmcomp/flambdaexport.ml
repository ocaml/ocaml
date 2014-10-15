(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ext_types
open Symbol
open Abstract_identifiers
open Flambda

module Innerid = Id(struct end)
module ExportId = UnitId(Innerid)(Compilation_unit)
module EidMap = ExtMap(ExportId)
module EidSet = ExtSet(ExportId)
module EidTbl = ExtHashtbl(ExportId)

type tag = int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_closure of value_offset
  | Value_unoffseted_closure of value_closure

and value_offset =
  { fun_id : function_within_closure;
    closure : value_closure }

and value_closure =
  { closure_id : FunId.t;
    bound_var : approx ClosureVariableMap.t;
    results : approx ClosureFunctionMap.t }

and approx =
  | Value_unknown
  | Value_id of ExportId.t
  | Value_symbol of Symbol.t

type exported = {
  ex_functions : unit function_declarations FunMap.t;
  ex_functions_off : unit function_declarations ClosureFunctionMap.t;
  ex_values : descr EidMap.t;
  ex_globals : approx IdentMap.t;
  ex_id_symbol : Symbol.t EidMap.t;
  ex_symbol_id : ExportId.t SymbolMap.t;
  ex_offset_fun : int ClosureFunctionMap.t;
  ex_offset_fv : int ClosureVariableMap.t;
  ex_constants : SymbolSet.t;
  ex_constant_closures : FunSet.t;
  ex_kept_arguments : VarSet.t FunMap.t;
}

let empty_export = {
  ex_functions = FunMap.empty;
  ex_functions_off = ClosureFunctionMap.empty;
  ex_values = EidMap.empty;
  ex_globals = IdentMap.empty;
  ex_id_symbol = EidMap.empty;
  ex_symbol_id = SymbolMap.empty;
  ex_offset_fun = ClosureFunctionMap.empty;
  ex_offset_fv = ClosureVariableMap.empty;
  ex_constants = SymbolSet.empty;
  ex_constant_closures = FunSet.empty;
  ex_kept_arguments = FunMap.empty;
}

let print_approx ppf export =
  let values = export.ex_values in
  let open Format in
  let printed = ref EidSet.empty in
  let printed_closure = ref FunSet.empty in
  let rec print_approx ppf = function
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if EidSet.mem id !printed
      then fprintf ppf "(%a: _)" ExportId.print id
      else
        (try
           let descr = EidMap.find id values in
           printed := EidSet.add id !printed;
           fprintf ppf "(%a: %a)"
             ExportId.print id
             print_descr descr
         with Not_found ->
           fprintf ppf "(%a: Not available)"
             ExportId.print id)
    | Value_symbol sym -> Symbol.print ppf sym
  and print_descr ppf = function
    | Value_int i -> pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) -> fprintf ppf "[%i:%a]" tag print_fields fields
    | Value_closure {fun_id; closure} ->
      fprintf ppf "(function %a, %a)" Closure_function.print fun_id print_closure closure
    | Value_unoffseted_closure closure ->
      fprintf ppf "(ufunction %a)" print_closure closure
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_closure ppf { closure_id; bound_var } =
    if FunSet.mem closure_id !printed_closure
    then fprintf ppf "%a" FunId.print closure_id
    else begin
      printed_closure := FunSet.add closure_id !printed_closure;
      fprintf ppf "{%a: %a}"
        FunId.print closure_id
        print_binding bound_var
    end
  and print_binding ppf bound_var =
    ClosureVariableMap.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Closure_variable.print clos_id
          print_approx approx) bound_var
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  IdentMap.iter print_approxs export.ex_globals

let print_symbols ppf export =
  let open Format in
  let print_symbol eid sym =
    fprintf ppf "%a -> %a@." Symbol.print sym ExportId.print eid
  in
  EidMap.iter print_symbol export.ex_id_symbol

let print_all ppf export =
  let open Format in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (EidMap.print Symbol.print) export.ex_id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (SymbolMap.print ExportId.print) export.ex_symbol_id;
  fprintf ppf "constants@ %a@.@."
    SymbolSet.print export.ex_constants;
  fprintf ppf "functions@ %a@.@."
    (FunMap.print Printflambda.function_declarations) export.ex_functions


let merge e1 e2 =
  let int_eq (i:int) j = i = j in
  { ex_values = EidMap.disjoint_union e1.ex_values e2.ex_values;
    ex_globals = IdentMap.disjoint_union e1.ex_globals e2.ex_globals;
    ex_functions = FunMap.disjoint_union e1.ex_functions e2.ex_functions;
    ex_functions_off =
      ClosureFunctionMap.disjoint_union e1.ex_functions_off e2.ex_functions_off;
    ex_id_symbol = EidMap.disjoint_union e1.ex_id_symbol e2.ex_id_symbol;
    ex_symbol_id = SymbolMap.disjoint_union e1.ex_symbol_id e2.ex_symbol_id;
    ex_offset_fun = ClosureFunctionMap.disjoint_union
        ~eq:int_eq e1.ex_offset_fun e2.ex_offset_fun;
    ex_offset_fv = ClosureVariableMap.disjoint_union
        ~eq:int_eq e1.ex_offset_fv e2.ex_offset_fv;
    ex_constants = SymbolSet.union e1.ex_constants e2.ex_constants;
    ex_constant_closures =
      FunSet.union e1.ex_constant_closures e2.ex_constant_closures;
    ex_kept_arguments =
      FunMap.disjoint_union e1.ex_kept_arguments e2.ex_kept_arguments }

(* importing informations to build a pack: the global identifying the
   compilation unit of symbols is changed to be the pack one *)

let rename_id_state = EidTbl.create 100

let import_eid_for_pack units pack id =
  try EidTbl.find rename_id_state id
  with Not_found ->
    let unit_id = ExportId.unit id in
    let id' =
      if CompilationUnitSet.mem unit_id units
      then
        ExportId.create ?name:(ExportId.name id) pack
      else id in
    EidTbl.add rename_id_state id id';
    id'

let import_symbol_for_pack units pack symbol =
  let unit = symbol.sym_unit in
  if CompilationUnitSet.mem unit units
  then { symbol with sym_unit = pack }
  else symbol

let import_approx_for_pack units pack = function
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_closure units pack closure =
  { closure_id = closure.closure_id;
    bound_var =
      ClosureVariableMap.map (import_approx_for_pack units pack) closure.bound_var;
    results =
      ClosureFunctionMap.map (import_approx_for_pack units pack) closure.results }

let import_descr_for_pack units pack = function
  | Value_int _
  | Value_constptr _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure {fun_id; closure} ->
    Value_closure {fun_id; closure = import_closure units pack closure}
  | Value_unoffseted_closure closure ->
    Value_unoffseted_closure (import_closure units pack closure)

let import_code_for_pack units pack expr =
  Flambdaiter.map (function
      | Fsymbol (sym, ()) ->
        Fsymbol (import_symbol_for_pack units pack sym, ())
      | e -> e)
    expr

let import_ffunctions_for_pack units pack ffuns =
  { ffuns with
    funs = VarMap.map (fun ffun ->
        {ffun with body = import_code_for_pack units pack ffun.body})
        ffuns.funs }

let ex_functions_off ex_functions =
  let aux_fun ffunctions function_id _ map =
    ClosureFunctionMap.add
      (Closure_function.wrap function_id) ffunctions map in
  let aux _ f map = VarMap.fold (aux_fun f) f.funs map in
  FunMap.fold aux ex_functions ClosureFunctionMap.empty

let import_eidmap_for_pack units pack f map =
  EidMap.map_keys
    (import_eid_for_pack units pack)
    (EidMap.map f map)

let import_for_pack ~pack_units ~pack exp =
  let import_sym = import_symbol_for_pack pack_units pack in
  let import_desr = import_descr_for_pack pack_units pack in
  let import_approx = import_approx_for_pack pack_units pack in
  let import_eid = import_eid_for_pack pack_units pack in
  let import_eidmap f map = import_eidmap_for_pack pack_units pack f map in
  let ex_functions =
    FunMap.map (import_ffunctions_for_pack pack_units pack)
      exp.ex_functions in
  (* The only reachable global identifier of a pack is the pack itself *)
  let globals = IdentMap.filter (fun unit _ ->
      Ident.same (Compilation_unit.get_persistent_ident pack) unit)
      exp.ex_globals in
  let res =
    { ex_functions;
      ex_functions_off = ex_functions_off ex_functions;
      ex_globals = IdentMap.map import_approx globals;
      ex_offset_fun = exp.ex_offset_fun;
      ex_offset_fv = exp.ex_offset_fv;
      ex_values = import_eidmap import_desr exp.ex_values;
      ex_id_symbol = import_eidmap import_sym exp.ex_id_symbol;
      ex_symbol_id = SymbolMap.map_keys import_sym
          (SymbolMap.map import_eid exp.ex_symbol_id);
      ex_constants = SymbolSet.map import_sym exp.ex_constants;
      ex_constant_closures = exp.ex_constant_closures;
      ex_kept_arguments = exp.ex_kept_arguments } in
  EidTbl.clear rename_id_state;
  res
