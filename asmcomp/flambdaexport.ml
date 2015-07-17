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

module ET (* phone home *) = Flambdaexport_types

let empty_export : ET.exported = {
  ex_functions = Set_of_closures_id.Map.empty;
  ex_functions_off = Closure_id.Map.empty;
  ex_values =  Compilation_unit.Map.empty;
  ex_globals = Ident.Map.empty;
  ex_id_symbol =  Compilation_unit.Map.empty;
  ex_symbol_id = Symbol.Map.empty;
  ex_offset_fun = Closure_id.Map.empty;
  ex_offset_fv = Var_within_closure.Map.empty;
  ex_constants = Symbol.Set.empty;
  ex_constant_closures = Set_of_closures_id.Set.empty;
  ex_invariant_arguments = Set_of_closures_id.Map.empty;
}

let find_ex_value eid map =
  let unit = Export_id.unit eid in
  let unit_map = Compilation_unit.Map.find unit map in
  Export_id.Map.find eid unit_map

let find_description eid (ex : ET.exported) =
  find_ex_value eid ex.ex_values

let eidmap_disjoint_union m1 m2 =
  Compilation_unit.Map.merge
    (fun _id x y -> match x, y with
       | None, None -> None
       | None, Some v
       | Some v, None -> Some v
       | Some v1, Some v2 ->
           Some (Export_id.Map.disjoint_union v1 v2))
    m1 m2

let nest_eid_map map =
  let add_map eid v map =
    let unit = Export_id.unit eid in
    let m = try Compilation_unit.Map.find unit map
      with Not_found -> Export_id.Map.empty in
    Compilation_unit.Map.add unit (Export_id.Map.add eid v m) map
  in
  Export_id.Map.fold add_map map Compilation_unit.Map.empty

let print_approx ppf (export : ET.exported) =
  let values = export.ex_values in
  let fprintf = Format.fprintf in
  let printed = ref Export_id.Set.empty in
  let printed_set_of_closures = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf (approx : ET.approx) =
    match approx with
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if Export_id.Set.mem id !printed
      then fprintf ppf "(%a: _)" Export_id.print id
      else
        (try
           let descr = find_ex_value id values in
           printed := Export_id.Set.add id !printed;
           fprintf ppf "(%a: %a)"
             Export_id.print id
             print_descr descr
         with Not_found ->
           fprintf ppf "(%a: Not available)"
             Export_id.print id)
    | Value_symbol sym -> Symbol.print ppf sym
  and print_descr ppf (descr : ET.descr) =
    match descr with
    | Value_int i -> Format.pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%a:%a]" Tag.print tag
        print_fields fields
    | Value_mutable_block (tag, size) ->
      fprintf ppf "[mutable %a:%i]" Tag.print tag size
    | Value_closure {fun_id; set_of_closures} ->
      fprintf ppf "(closure %a, %a)" Closure_id.print fun_id
        print_set_of_closures set_of_closures
    | Value_set_of_closures set_of_closures ->
      fprintf ppf "(set_of_closures %a)" print_set_of_closures set_of_closures
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
    | ET.Value_boxed_int (t, i) ->
      let module A = Simple_value_approx in
      match t with
      | A.Int32 -> Format.fprintf ppf "%li" i
      | A.Int64 -> Format.fprintf ppf "%Li" i
      | A.Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_set_of_closures ppf { ET. set_of_closures_id; bound_vars } =
    if Set_of_closures_id.Set.mem set_of_closures_id !printed_set_of_closures
    then fprintf ppf "%a" Set_of_closures_id.print set_of_closures_id
    else begin
      printed_set_of_closures
        := Set_of_closures_id.Set.add set_of_closures_id !printed_set_of_closures;
      fprintf ppf "{%a: %a}"
        Set_of_closures_id.print set_of_closures_id
        print_binding bound_vars
    end
  and print_binding ppf bound_vars =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx) bound_vars
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  Ident.Map.iter print_approxs export.ex_globals

let print_symbols ppf (export : ET.exported) =
  let print_symbol eid sym =
    Format.fprintf ppf "%a -> %a@." Symbol.print sym Export_id.print eid
  in
  Compilation_unit.Map.iter (fun _ -> Export_id.Map.iter print_symbol)
    export.ex_id_symbol

let print_offsets ppf (export : ET.exported) =
  Format.fprintf ppf "@[<v 2>offset_fun:@ ";
  Closure_id.Map.iter (fun cid off ->
      Format.fprintf ppf "%a -> %i@ "
        Closure_id.print cid off) export.ex_offset_fun;
  Format.fprintf ppf "@]@ @[<v 2>offset_fv:@ ";
  Var_within_closure.Map.iter (fun vid off ->
      Format.fprintf ppf "%a -> %i@ "
        Var_within_closure.print vid off) export.ex_offset_fv;
  Format.fprintf ppf "@]@ "

let print_all ppf (export : ET.exported) =
  let fprintf = Format.fprintf in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (Export_id.Map.print Symbol.print)) export.ex_id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (Symbol.Map.print Export_id.print) export.ex_symbol_id;
  fprintf ppf "constants@ %a@.@."
    Symbol.Set.print export.ex_constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Flambda_printers.function_declarations) export.ex_functions

let merge (e1 : ET.exported) (e2 : ET.exported) : ET.exported =
  let int_eq (i:int) j = i = j in
  { ex_values = eidmap_disjoint_union e1.ex_values e2.ex_values;
    ex_globals = Ident.Map.disjoint_union e1.ex_globals e2.ex_globals;
    ex_functions = Set_of_closures_id.Map.disjoint_union e1.ex_functions e2.ex_functions;
    ex_functions_off =
      Closure_id.Map.disjoint_union e1.ex_functions_off e2.ex_functions_off;
    ex_id_symbol = eidmap_disjoint_union  e1.ex_id_symbol e2.ex_id_symbol;
    ex_symbol_id = Symbol.Map.disjoint_union e1.ex_symbol_id e2.ex_symbol_id;
    ex_offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq e1.ex_offset_fun e2.ex_offset_fun;
    ex_offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq e1.ex_offset_fv e2.ex_offset_fv;
    ex_constants = Symbol.Set.union e1.ex_constants e2.ex_constants;
    ex_constant_closures =
      Set_of_closures_id.Set.union e1.ex_constant_closures e2.ex_constant_closures;
    ex_invariant_arguments =
      Set_of_closures_id.Map.disjoint_union
        e1.ex_invariant_arguments e2.ex_invariant_arguments;
  }

(* importing informations to build a pack: the global identifying the
   compilation unit of symbols is changed to be the pack one *)

let rename_id_state = Export_id.Tbl.create 100

let import_eid_for_pack units pack id =
  try Export_id.Tbl.find rename_id_state id
  with Not_found ->
    let unit_id = Export_id.unit id in
    let id' =
      if Compilation_unit.Set.mem unit_id units
      then
        Export_id.create ?name:(Export_id.name id) pack
      else id in
    Export_id.Tbl.add rename_id_state id id';
    id'

let import_symbol_for_pack units pack symbol =
  let compilation_unit = Symbol.compilation_unit symbol in
  if Compilation_unit.Set.mem compilation_unit units
  then Symbol.create pack (Symbol.label symbol)
  else symbol

let import_approx_for_pack units pack (approx : ET.approx) : ET.approx =
  match approx with
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_set_of_closures units pack
      (set_of_closures : ET.value_set_of_closures) : ET.value_set_of_closures =
  { set_of_closures_id = set_of_closures.set_of_closures_id;
    bound_vars =
      Var_within_closure.Map.map (import_approx_for_pack units pack)
        set_of_closures.bound_vars;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack)
        set_of_closures.results }

let import_descr_for_pack units pack (descr : ET.descr) : ET.descr =
  match descr with
  | Value_int _
  | Value_constptr _
  | Value_string _
  | Value_float _
  | Value_float_array _
  | ET.Value_boxed_int _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure {fun_id; set_of_closures} ->
    Value_closure {
      fun_id;
      set_of_closures = import_set_of_closures units pack set_of_closures;
    }
  | Value_set_of_closures set_of_closures ->
    Value_set_of_closures (import_set_of_closures units pack set_of_closures)
  | Value_mutable_block (tag, size) ->
    Value_mutable_block (tag, size)

let import_code_for_pack units pack expr =
  Flambda_iterators.map_named (function
      | Symbol sym -> Symbol (import_symbol_for_pack units pack sym)
      | e -> e)
    expr

let import_ffunctions_for_pack units pack
      (ffuns : Flambda.function_declarations) =
  { ffuns with
    funs = Variable.Map.map (fun (ffun : Flambda.function_declaration) ->
        {ffun with body = import_code_for_pack units pack ffun.body})
        ffuns.funs }

let ex_functions_off ex_functions =
  let aux_fun ffunctions function_id _ map =
    Closure_id.Map.add
      (Closure_id.wrap function_id) ffunctions map in
  let aux _ (f : Flambda.function_declarations) map =
    Variable.Map.fold (aux_fun f) f.funs map
  in
  Set_of_closures_id.Map.fold aux ex_functions Closure_id.Map.empty


let import_eidmap_for_pack units pack f map =
  nest_eid_map
    (Compilation_unit.Map.fold
       (fun _ map acc -> Export_id.Map.disjoint_union map acc)
       (Compilation_unit.Map.map
          (fun map ->
             Export_id.Map.map_keys
            (import_eid_for_pack units pack)
            (Export_id.Map.map f map))
          map)
       Export_id.Map.empty)

let import_for_pack ~pack_units ~pack (exp : ET.exported) =
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
  let res : ET.exported =
    { ex_functions;
      ex_functions_off = ex_functions_off ex_functions;
      ex_globals = Ident.Map.map import_approx globals;
      ex_offset_fun = exp.ex_offset_fun;
      ex_offset_fv = exp.ex_offset_fv;
      ex_values = import_eidmap import_desr exp.ex_values;
      ex_id_symbol = import_eidmap import_sym exp.ex_id_symbol;
      ex_symbol_id = Symbol.Map.map_keys import_sym
          (Symbol.Map.map import_eid exp.ex_symbol_id);
      ex_constants = Symbol.Set.map import_sym exp.ex_constants;
      ex_constant_closures = exp.ex_constant_closures;
      ex_invariant_arguments = exp.ex_invariant_arguments } in
  res

let clear_import_state () = Export_id.Tbl.clear rename_id_state
