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

type _ boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = {
  contents : string option; (* None if unknown or mutable *)
  size : int;
}

type descr =
  | Value_block of tag * approx array
  | Value_mutable_block of tag * int
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of int
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_offset
  | Value_set_of_closures of value_closure

and value_offset =
  { fun_id : Closure_id.t;
    closure : value_closure }

and value_closure =
  { closure_id : Set_of_closures_id.t;
    bound_var : approx Var_within_closure.Map.t;
    results : approx Closure_id.Map.t }

and approx =
  | Value_unknown
  | Value_id of ExportId.t
  | Value_symbol of Symbol.t

type exported = {
  ex_functions : unit function_declarations Set_of_closures_id.Map.t;
  ex_functions_off : unit function_declarations Closure_id.Map.t;
  ex_values : descr EidMap.t Compilation_unit.Map.t;
  ex_globals : approx Ident.Map.t;
  ex_id_symbol : Symbol.t EidMap.t Compilation_unit.Map.t;
  ex_symbol_id : ExportId.t SymbolMap.t;
  ex_offset_fun : int Closure_id.Map.t;
  ex_offset_fv : int Var_within_closure.Map.t;
  ex_constants : SymbolSet.t;
  ex_constant_closures : Set_of_closures_id.Set.t;
  ex_kept_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

let empty_export = {
  ex_functions = Set_of_closures_id.Map.empty;
  ex_functions_off = Closure_id.Map.empty;
  ex_values =  Compilation_unit.Map.empty;
  ex_globals = Ident.Map.empty;
  ex_id_symbol =  Compilation_unit.Map.empty;
  ex_symbol_id = SymbolMap.empty;
  ex_offset_fun = Closure_id.Map.empty;
  ex_offset_fv = Var_within_closure.Map.empty;
  ex_constants = SymbolSet.empty;
  ex_constant_closures = Set_of_closures_id.Set.empty;
  ex_kept_arguments = Set_of_closures_id.Map.empty;
}

let find_ex_value eid map =
  let unit = ExportId.unit eid in
  let unit_map = Compilation_unit.Map.find unit map in
  EidMap.find eid unit_map

let find_description eid ex = find_ex_value eid ex.ex_values

let eidmap_disjoint_union m1 m2 =
  Compilation_unit.Map.merge
    (fun _id x y -> match x, y with
       | None, None -> None
       | None, Some v
       | Some v, None -> Some v
       | Some v1, Some v2 ->
           Some (EidMap.disjoint_union v1 v2))
    m1 m2

let nest_eid_map map =
  let add_map eid v map =
    let unit = ExportId.unit eid in
    let m = try Compilation_unit.Map.find unit map
      with Not_found -> EidMap.empty in
    Compilation_unit.Map.add unit (EidMap.add eid v m) map
  in
  EidMap.fold add_map map Compilation_unit.Map.empty

let print_approx ppf export =
  let values = export.ex_values in
  let open Format in
  let printed = ref EidSet.empty in
  let printed_closure = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf = function
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if EidSet.mem id !printed
      then fprintf ppf "(%a: _)" ExportId.print id
      else
        (try
           let descr = find_ex_value id values in
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
    | Value_mutable_block (tag, size) -> fprintf ppf "[mutable %i:%i]" tag size
    | Value_closure {fun_id; closure} ->
      fprintf ppf "(function %a, %a)" Closure_id.print fun_id print_closure closure
    | Value_set_of_closures closure ->
      fprintf ppf "(ufunction %a)" print_closure closure
    | Value_string { contents; size } -> begin
        match contents with
        | None ->
            Format.fprintf ppf "string %i" size
        | Some s ->
            let s =
              if size > 10
              then String.sub s 0 8 ^ "..."
              else s
            in
            Format.fprintf ppf "string %i %S" size s
      end
    | Value_float f -> Format.pp_print_float ppf f
    | Value_float_array size ->
        Format.fprintf ppf "float_array %i" size
    | Value_boxed_int (t, i) ->
      match t with
      | Int32 -> Format.fprintf ppf "%li" i
      | Int64 -> Format.fprintf ppf "%Li" i
      | Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_closure ppf { closure_id; bound_var } =
    if Set_of_closures_id.Set.mem closure_id !printed_closure
    then fprintf ppf "%a" Set_of_closures_id.print closure_id
    else begin
      printed_closure := Set_of_closures_id.Set.add closure_id !printed_closure;
      fprintf ppf "{%a: %a}"
        Set_of_closures_id.print closure_id
        print_binding bound_var
    end
  and print_binding ppf bound_var =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx) bound_var
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  Ident.Map.iter print_approxs export.ex_globals

let print_symbols ppf export =
  let open Format in
  let print_symbol eid sym =
    fprintf ppf "%a -> %a@." Symbol.print sym ExportId.print eid
  in
   Compilation_unit.Map.iter (fun _ -> EidMap.iter print_symbol) export.ex_id_symbol

let print_offsets ppf export =
  Format.fprintf ppf "@[<v 2>offset_fun:@ ";
  Closure_id.Map.iter (fun cid off ->
      Format.fprintf ppf "%a -> %i@ "
        Closure_id.print cid off) export.ex_offset_fun;
  Format.fprintf ppf "@]@ @[<v 2>offset_fv:@ ";
  Var_within_closure.Map.iter (fun vid off ->
      Format.fprintf ppf "%a -> %i@ "
        Var_within_closure.print vid off) export.ex_offset_fv;
  Format.fprintf ppf "@]@ "

let print_all ppf export =
  let open Format in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (EidMap.print Symbol.print)) export.ex_id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (SymbolMap.print ExportId.print) export.ex_symbol_id;
  fprintf ppf "constants@ %a@.@."
    SymbolSet.print export.ex_constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Printflambda.function_declarations) export.ex_functions


let merge e1 e2 =
  let int_eq (i:int) j = i = j in
  { ex_values = eidmap_disjoint_union e1.ex_values e2.ex_values;
    ex_globals = Ident.Map.disjoint_union e1.ex_globals e2.ex_globals;
    ex_functions = Set_of_closures_id.Map.disjoint_union e1.ex_functions e2.ex_functions;
    ex_functions_off =
      Closure_id.Map.disjoint_union e1.ex_functions_off e2.ex_functions_off;
    ex_id_symbol = eidmap_disjoint_union  e1.ex_id_symbol e2.ex_id_symbol;
    ex_symbol_id = SymbolMap.disjoint_union e1.ex_symbol_id e2.ex_symbol_id;
    ex_offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq e1.ex_offset_fun e2.ex_offset_fun;
    ex_offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq e1.ex_offset_fv e2.ex_offset_fv;
    ex_constants = SymbolSet.union e1.ex_constants e2.ex_constants;
    ex_constant_closures =
      Set_of_closures_id.Set.union e1.ex_constant_closures e2.ex_constant_closures;
    ex_kept_arguments =
      Set_of_closures_id.Map.disjoint_union e1.ex_kept_arguments e2.ex_kept_arguments }

(* importing informations to build a pack: the global identifying the
   compilation unit of symbols is changed to be the pack one *)

let rename_id_state = EidTbl.create 100

let import_eid_for_pack units pack id =
  try EidTbl.find rename_id_state id
  with Not_found ->
    let unit_id = ExportId.unit id in
    let id' =
      if Compilation_unit.Set.mem unit_id units
      then
        ExportId.create ?name:(ExportId.name id) pack
      else id in
    EidTbl.add rename_id_state id id';
    id'

let import_symbol_for_pack units pack symbol =
  let unit = symbol.sym_unit in
  if Compilation_unit.Set.mem unit units
  then { symbol with sym_unit = pack }
  else symbol

let import_approx_for_pack units pack = function
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_closure units pack closure =
  { closure_id = closure.closure_id;
    bound_var =
      Var_within_closure.Map.map (import_approx_for_pack units pack) closure.bound_var;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack) closure.results }

let import_descr_for_pack units pack = function
  | Value_int _
  | Value_constptr _
  | Value_string _
  | Value_float _
  | Value_float_array _
  | Value_boxed_int _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure {fun_id; closure} ->
    Value_closure {fun_id; closure = import_closure units pack closure}
  | Value_set_of_closures closure ->
    Value_set_of_closures (import_closure units pack closure)
  | Value_mutable_block (tag, size) ->
    Value_mutable_block (tag, size)

let import_code_for_pack units pack expr =
  Flambdaiter.map (function
      | Fsymbol (sym, ()) ->
        Fsymbol (import_symbol_for_pack units pack sym, ())
      | e -> e)
    expr

let import_ffunctions_for_pack units pack ffuns =
  { ffuns with
    funs = Variable.Map.map (fun ffun ->
        {ffun with body = import_code_for_pack units pack ffun.body})
        ffuns.funs }

let ex_functions_off ex_functions =
  let aux_fun ffunctions function_id _ map =
    Closure_id.Map.add
      (Closure_id.wrap function_id) ffunctions map in
  let aux _ f map = Variable.Map.fold (aux_fun f) f.funs map in
  Set_of_closures_id.Map.fold aux ex_functions Closure_id.Map.empty


let import_eidmap_for_pack units pack f map =
  nest_eid_map
    (Compilation_unit.Map.fold
       (fun _ map acc -> EidMap.disjoint_union map acc)
       (Compilation_unit.Map.map
          (fun map ->
             EidMap.map_keys
            (import_eid_for_pack units pack)
            (EidMap.map f map))
          map)
       EidMap.empty)

let import_for_pack ~pack_units ~pack exp =
  let import_sym = import_symbol_for_pack pack_units pack in
  let import_desr = import_descr_for_pack pack_units pack in
  let import_approx = import_approx_for_pack pack_units pack in
  let import_eid = import_eid_for_pack pack_units pack in
  let import_eidmap f map = import_eidmap_for_pack pack_units pack f map in
  let ex_functions =
    Set_of_closures_id.Map.map (import_ffunctions_for_pack pack_units pack)
      exp.ex_functions in
  (* The only reachable global identifier of a pack is the pack itself *)
  let globals = Ident.Map.filter (fun unit _ ->
      Ident.same (Compilation_unit.get_persistent_ident pack) unit)
      exp.ex_globals in
  let res =
    { ex_functions;
      ex_functions_off = ex_functions_off ex_functions;
      ex_globals = Ident.Map.map import_approx globals;
      ex_offset_fun = exp.ex_offset_fun;
      ex_offset_fv = exp.ex_offset_fv;
      ex_values = import_eidmap import_desr exp.ex_values;
      ex_id_symbol = import_eidmap import_sym exp.ex_id_symbol;
      ex_symbol_id = SymbolMap.map_keys import_sym
          (SymbolMap.map import_eid exp.ex_symbol_id);
      ex_constants = SymbolSet.map import_sym exp.ex_constants;
      ex_constant_closures = exp.ex_constant_closures;
      ex_kept_arguments = exp.ex_kept_arguments } in
  res

let clear_import_state () = EidTbl.clear rename_id_state
