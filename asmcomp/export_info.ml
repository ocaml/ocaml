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
  aliased_symbol : Symbol.t option;
}

and approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

type t = {
  sets_of_closures : Flambda.function_declarations Set_of_closures_id.Map.t;
  closures : Flambda.function_declarations Closure_id.Map.t;
  values : descr Export_id.Map.t Compilation_unit.Map.t;
  globals : approx Ident.Map.t;
  id_symbol : Symbol.t Export_id.Map.t Compilation_unit.Map.t;
  symbol_id : Export_id.t Symbol.Map.t;
  offset_fun : int Closure_id.Map.t;
  offset_fv : int Var_within_closure.Map.t;
  constants : Symbol.Set.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
  invariant_arguments : Variable.Set.t Set_of_closures_id.Map.t;
}

let empty : t = {
  sets_of_closures = Set_of_closures_id.Map.empty;
  closures = Closure_id.Map.empty;
  values = Compilation_unit.Map.empty;
  globals = Ident.Map.empty;
  id_symbol = Compilation_unit.Map.empty;
  symbol_id = Symbol.Map.empty;
  offset_fun = Closure_id.Map.empty;
  offset_fv = Var_within_closure.Map.empty;
  constants = Symbol.Set.empty;
  constant_sets_of_closures = Set_of_closures_id.Set.empty;
  invariant_arguments = Set_of_closures_id.Map.empty;
}

let create ~sets_of_closures ~closures ~values ~globals ~id_symbol
      ~symbol_id ~offset_fun ~offset_fv ~constants ~constant_sets_of_closures
      ~invariant_arguments =
  { sets_of_closures;
    closures;
    values;
    globals;
    id_symbol;
    symbol_id;
    offset_fun;
    offset_fv;
    constants;
    constant_sets_of_closures;
    invariant_arguments;
  }

let add_offsets t ~offset_fun ~offset_fv =
  { t with offset_fun; offset_fv }

let merge (t1 : t) (t2 : t) : t =
  let eidmap_disjoint_union map1 map2 =
    Compilation_unit.Map.merge (fun _id map1 map2 ->
        match map1, map2 with
        | None, None -> None
        | None, Some map
        | Some map, None -> Some map
        | Some map1, Some map2 -> Some (Export_id.Map.disjoint_union map1 map2))
      map1 map2
  in
  let int_eq (i : int) j = i = j in
  { values = eidmap_disjoint_union t1.values t2.values;
    globals = Ident.Map.disjoint_union t1.globals t2.globals;
    sets_of_closures =
      Set_of_closures_id.Map.disjoint_union t1.sets_of_closures
        t2.sets_of_closures;
    closures = Closure_id.Map.disjoint_union t1.closures t2.closures;
    id_symbol = eidmap_disjoint_union t1.id_symbol t2.id_symbol;
    symbol_id = Symbol.Map.disjoint_union t1.symbol_id t2.symbol_id;
    offset_fun = Closure_id.Map.disjoint_union
        ~eq:int_eq t1.offset_fun t2.offset_fun;
    offset_fv = Var_within_closure.Map.disjoint_union
        ~eq:int_eq t1.offset_fv t2.offset_fv;
    constants = Symbol.Set.union t1.constants t2.constants;
    constant_sets_of_closures =
      Set_of_closures_id.Set.union t1.constant_sets_of_closures
        t2.constant_sets_of_closures;
    invariant_arguments =
      Set_of_closures_id.Map.disjoint_union
        t1.invariant_arguments t2.invariant_arguments;
  }

let find_value eid map =
  let unit_map = Compilation_unit.Map.find (Export_id.unit eid) map in
  Export_id.Map.find eid unit_map

let find_description (t : t) eid =
  find_value eid t.values

let nest_eid_map map =
  let add_map eid v map =
    let unit = Export_id.unit eid in
    let m =
      try Compilation_unit.Map.find unit map
      with Not_found -> Export_id.Map.empty
    in
    Compilation_unit.Map.add unit (Export_id.Map.add eid v m) map
  in
  Export_id.Map.fold add_map map Compilation_unit.Map.empty

let print_approx ppf (t : t) =
  let values = t.values in
  let fprintf = Format.fprintf in
  let printed = ref Export_id.Set.empty in
  let recorded_symbol = ref Symbol.Set.empty in
  let symbols_to_print = Queue.create () in
  let printed_set_of_closures = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf (approx : approx) =
    match approx with
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if Export_id.Set.mem id !printed then
        fprintf ppf "(%a: _)" Export_id.print id
      else begin
        try
          let descr = find_value id values in
          printed := Export_id.Set.add id !printed;
          fprintf ppf "@[<hov 2>(%a:@ %a)@]" Export_id.print id print_descr descr
        with Not_found ->
          fprintf ppf "(%a: Not available)" Export_id.print id
      end
    | Value_symbol sym ->
      if not (Symbol.Set.mem sym !recorded_symbol) then begin
        recorded_symbol := Symbol.Set.add sym !recorded_symbol;
        Queue.push sym symbols_to_print;
      end;
      Symbol.print ppf sym
  and print_descr ppf (descr : descr) =
    match descr with
    | Value_int i -> Format.pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%a:%a]" Tag.print tag print_fields fields
    | Value_mutable_block (tag, size) ->
      fprintf ppf "[mutable %a:%i]" Tag.print tag size
    | Value_closure {closure_id; set_of_closures} ->
      fprintf ppf "(closure %a, %a)" Closure_id.print closure_id
        print_set_of_closures set_of_closures
    | Value_set_of_closures set_of_closures ->
      fprintf ppf "(set_of_closures %a)" print_set_of_closures set_of_closures
    | Value_string { contents; size } ->
      begin match contents with
      | Unknown_or_mutable -> Format.fprintf ppf "string %i" size
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
  and print_set_of_closures ppf
      { set_of_closures_id; bound_vars; aliased_symbol } =
    if Set_of_closures_id.Set.mem set_of_closures_id !printed_set_of_closures
    then fprintf ppf "%a" Set_of_closures_id.print set_of_closures_id
    else begin
      printed_set_of_closures :=
        Set_of_closures_id.Set.add set_of_closures_id !printed_set_of_closures;
      let print_alias ppf = function
        | None -> ()
        | Some symbol ->
          Format.fprintf ppf "@ (alias: %a)" Symbol.print symbol
      in
      fprintf ppf "{%a: %a%a}"
        Set_of_closures_id.print set_of_closures_id
        print_binding bound_vars
        print_alias aliased_symbol
    end
  and print_binding ppf bound_vars =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx)
      bound_vars
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  let rec print_recorded_symbols () =
    if not (Queue.is_empty symbols_to_print) then begin
      let sym = Queue.pop symbols_to_print in
      begin match Symbol.Map.find sym t.symbol_id with
      | exception Not_found -> ()
      | id ->
        fprintf ppf "@[<hov 2>%a:@ %a@];@ "
          Symbol.print sym
          print_approx (Value_id id)
      end;
      print_recorded_symbols ();
    end
  in
  fprintf ppf "@[<hov 2>Globals:@ ";
  Ident.Map.iter print_approxs t.globals;
  fprintf ppf "@]@ @[<hov 2>Symbols:@ ";
  print_recorded_symbols ();
  fprintf ppf "@]"

let print_symbols ppf (t : t) =
  let print_symbol eid sym =
    Format.fprintf ppf "%a -> %a@." Symbol.print sym Export_id.print eid
  in
  Compilation_unit.Map.iter (fun _ -> Export_id.Map.iter print_symbol)
    t.id_symbol

let print_offsets ppf (t : t) =
  Format.fprintf ppf "@[<v 2>offset_fun:@ ";
  Closure_id.Map.iter (fun cid off ->
      Format.fprintf ppf "%a -> %i@ "
        Closure_id.print cid off) t.offset_fun;
  Format.fprintf ppf "@]@ @[<v 2>offset_fv:@ ";
  Var_within_closure.Map.iter (fun vid off ->
      Format.fprintf ppf "%a -> %i@ "
        Var_within_closure.print vid off) t.offset_fv;
  Format.fprintf ppf "@]@ "

let print_all ppf (t : t) =
  let fprintf = Format.fprintf in
  fprintf ppf "approxs@ %a@.@."
    print_approx t;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (Export_id.Map.print Symbol.print))
    t.id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (Symbol.Map.print Export_id.print) t.symbol_id;
  fprintf ppf "constants@ %a@.@."
    Symbol.Set.print t.constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Flambda.print_function_declarations)
    t.sets_of_closures
