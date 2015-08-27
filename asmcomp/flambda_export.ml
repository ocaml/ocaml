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

type value_string_contents =
  | Contents of string
  | Unknown_or_mutable

type value_string = {
  contents : value_string_contents;
  size : int;
}

type descr =
  | Value_block of Tag.t * approx array
  | Value_mutable_block of Tag.t * int
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_float_array of int
  | Value_boxed_int : 'a Simple_value_approx.boxed_int * 'a -> descr
  | Value_string of value_string
  | Value_closure of value_closure
  | Value_set_of_closures of value_set_of_closures

and value_closure = {
  closure_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
}

and value_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  bound_vars : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

type exported = {
  functions : Flambda.function_declarations Set_of_closures_id.Map.t;
  functions_off : Flambda.function_declarations Closure_id.Map.t;
  values : descr Export_id.Map.t Compilation_unit.Map.t;
  globals : approx Ident.Map.t;
  id_symbol : Symbol.t Export_id.Map.t Compilation_unit.Map.t;
  symbol_id : Export_id.t Symbol.Map.t;
  offset_fun : int Closure_id.Map.t;
  offset_fv : int Var_within_closure.Map.t;
  constants : Symbol.Set.t;
  constant_closures : Set_of_closures_id.Set.t;
  invariant_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

let empty_export : exported = {
  functions = Set_of_closures_id.Map.empty;
  functions_off = Closure_id.Map.empty;
  values =  Compilation_unit.Map.empty;
  globals = Ident.Map.empty;
  id_symbol =  Compilation_unit.Map.empty;
  symbol_id = Symbol.Map.empty;
  offset_fun = Closure_id.Map.empty;
  offset_fv = Var_within_closure.Map.empty;
  constants = Symbol.Set.empty;
  constant_closures = Set_of_closures_id.Set.empty;
  invariant_arguments = Set_of_closures_id.Map.empty;
}

let create_exported ~functions ~functions_off ~values ~globals ~id_symbol
      ~symbol_id ~constant_closures ~invariant_arguments =
  { empty_export with
    functions;
    functions_off;
    values;
    globals;
    id_symbol;
    symbol_id;
    constant_closures;
    invariant_arguments;
  }

let find_ex_value eid map =
  let unit = Export_id.unit eid in
  let unit_map = Compilation_unit.Map.find unit map in
  Export_id.Map.find eid unit_map

let find_description eid (ex : exported) =
  find_ex_value eid ex.values

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

let print_approx ppf (export : exported) =
  let values = export.values in
  let fprintf = Format.fprintf in
  let printed = ref Export_id.Set.empty in
  let printed_set_of_closures = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf (approx : approx) =
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
  and print_descr ppf (descr : descr) =
    match descr with
    | Value_int i -> Format.pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%a:%a]" Tag.print tag
        print_fields fields
    | Value_mutable_block (tag, size) ->
      fprintf ppf "[mutable %a:%i]" Tag.print tag size
    | Value_closure {closure_id; set_of_closures} ->
      fprintf ppf "(closure %a, %a)" Closure_id.print closure_id
        print_set_of_closures set_of_closures
    | Value_set_of_closures set_of_closures ->
      fprintf ppf "(set_of_closures %a)" print_set_of_closures set_of_closures
    | Value_string { contents; size } -> begin
        match contents with
        | Unknown_or_mutable ->
            Format.fprintf ppf "string %i" size
        | Contents s ->
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
      let module A = Simple_value_approx in
      match t with
      | A.Int32 -> Format.fprintf ppf "%li" i
      | A.Int64 -> Format.fprintf ppf "%Li" i
      | A.Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_set_of_closures ppf {  set_of_closures_id; bound_vars } =
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
  Ident.Map.iter print_approxs export.globals

let print_symbols ppf (export : exported) =
  let print_symbol eid sym =
    Format.fprintf ppf "%a -> %a@." Symbol.print sym Export_id.print eid
  in
  Compilation_unit.Map.iter (fun _ -> Export_id.Map.iter print_symbol)
    export.id_symbol

let print_offsets ppf (export : exported) =
  Format.fprintf ppf "@[<v 2>offset_fun:@ ";
  Closure_id.Map.iter (fun cid off ->
      Format.fprintf ppf "%a -> %i@ "
        Closure_id.print cid off) export.offset_fun;
  Format.fprintf ppf "@]@ @[<v 2>offset_fv:@ ";
  Var_within_closure.Map.iter (fun vid off ->
      Format.fprintf ppf "%a -> %i@ "
        Var_within_closure.print vid off) export.offset_fv;
  Format.fprintf ppf "@]@ "

let print_all ppf (export : exported) =
  let fprintf = Format.fprintf in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (Export_id.Map.print Symbol.print)) export.id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (Symbol.Map.print Export_id.print) export.symbol_id;
  fprintf ppf "constants@ %a@.@."
    Symbol.Set.print export.constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Flambda.print_function_declarations) export.functions

let merge (e1 : exported) (e2 : exported) : exported =
  let int_eq (i:int) j = i = j in
  { values = eidmap_disjoint_union e1.values e2.values;
    globals = Ident.Map.disjoint_union e1.globals e2.globals;
    functions = Set_of_closures_id.Map.disjoint_union e1.functions e2.functions;
    functions_off =
      Closure_id.Map.disjoint_union e1.functions_off e2.functions_off;
    id_symbol = eidmap_disjoint_union  e1.id_symbol e2.id_symbol;
    symbol_id = Symbol.Map.disjoint_union e1.symbol_id e2.symbol_id;
    offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq e1.offset_fun e2.offset_fun;
    offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq e1.offset_fv e2.offset_fv;
    constants = Symbol.Set.union e1.constants e2.constants;
    constant_closures =
      Set_of_closures_id.Set.union e1.constant_closures e2.constant_closures;
    invariant_arguments =
      Set_of_closures_id.Map.disjoint_union
        e1.invariant_arguments e2.invariant_arguments;
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

let import_approx_for_pack units pack (approx : approx) : approx =
  match approx with
  | Value_symbol sym -> Value_symbol (import_symbol_for_pack units pack sym)
  | Value_id eid -> Value_id (import_eid_for_pack units pack eid)
  | Value_unknown -> Value_unknown

let import_set_of_closures units pack
      (set_of_closures : value_set_of_closures) : value_set_of_closures =
  { set_of_closures_id = set_of_closures.set_of_closures_id;
    bound_vars =
      Var_within_closure.Map.map (import_approx_for_pack units pack)
        set_of_closures.bound_vars;
    results =
      Closure_id.Map.map (import_approx_for_pack units pack)
        set_of_closures.results }

let import_descr_for_pack units pack (descr : descr) : descr =
  match descr with
  | Value_int _
  | Value_constptr _
  | Value_string _
  | Value_float _
  | Value_float_array _
  | Value_boxed_int _ as desc -> desc
  | Value_block (tag, fields) ->
    Value_block (tag, Array.map (import_approx_for_pack units pack) fields)
  | Value_closure {closure_id; set_of_closures} ->
    Value_closure {
      closure_id;
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
        Flambda.create_function_declaration ~params:ffun.params
          ~body:(import_code_for_pack units pack ffun.body)
          ~stub:ffun.stub ~dbg:ffun.dbg)
      ffuns.funs;
  }

let functions_off functions =
  let aux_fun ffunctions function_id _ map =
    Closure_id.Map.add
      (Closure_id.wrap function_id) ffunctions map in
  let aux _ (f : Flambda.function_declarations) map =
    Variable.Map.fold (aux_fun f) f.funs map
  in
  Set_of_closures_id.Map.fold aux functions Closure_id.Map.empty


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

let import_for_pack ~pack_units ~pack (exp : exported) =
  let import_sym = import_symbol_for_pack pack_units pack in
  let import_desr = import_descr_for_pack pack_units pack in
  let import_approx = import_approx_for_pack pack_units pack in
  let import_eid = import_eid_for_pack pack_units pack in
  let import_eidmap f map = import_eidmap_for_pack pack_units pack f map in
  let functions =
    Set_of_closures_id.Map.map (import_ffunctions_for_pack pack_units pack)
      exp.functions in
  (* The only reachable global identifier of a pack is the pack itself *)
  let globals = Ident.Map.filter (fun unit _ ->
      Ident.same (Compilation_unit.get_persistent_ident pack) unit)
      exp.globals in
  let res : exported =
    { functions;
      functions_off = functions_off functions;
      globals = Ident.Map.map import_approx globals;
      offset_fun = exp.offset_fun;
      offset_fv = exp.offset_fv;
      values = import_eidmap import_desr exp.values;
      id_symbol = import_eidmap import_sym exp.id_symbol;
      symbol_id = Symbol.Map.map_keys import_sym
          (Symbol.Map.map import_eid exp.symbol_id);
      constants = Symbol.Set.map import_sym exp.constants;
      constant_closures = exp.constant_closures;
      invariant_arguments = exp.invariant_arguments } in
  res

let clear_import_state () = Export_id.Tbl.clear rename_id_state
