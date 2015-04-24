(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Symbol
open Abstract_identifiers
open Flambda

type tag = int

type 'a boxed_int = 'a Flambdaexport.boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * t array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_offset
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t
  | Value_unresolved of Symbol.t

and value_offset =
  { fun_id : Closure_id.t;
    set_of_closures : value_set_of_closures;
    set_of_closures_var : Variable.t option; }

and value_set_of_closures =
  { ffunctions : Expr_id.t function_declarations;
    bound_var : t Var_within_closure.Map.t;
    unchanging_params : Variable.Set.t;
    specialised_args : Variable.Set.t;
    ffunction_sb :
      Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures.t;
  }

and t =
  { descr : descr;
    var : Variable.t option;
    symbol : Symbol.t option;
  }

let descr t = t.descr

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print_approx v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" tag p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Flambdaexport.ExportId.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { fun_id } ->
    Format.fprintf ppf "(fun:@ %a)" Closure_id.print fun_id
  | Value_set_of_closures { ffunctions = { funs } } ->
    Format.fprintf ppf "(set_of_closures:@ %a)"
      (fun ppf -> Variable.Map.iter (fun id _ -> Variable.print ppf id)) funs
  | Value_unresolved sym ->
    Format.fprintf ppf "(unresolved %a)" Symbol.print sym
  | Value_float f -> Format.pp_print_float ppf f
  | Value_boxed_int (t, i) ->
    match t with
    | Int32 -> Format.fprintf ppf "%li" i
    | Int64 -> Format.fprintf ppf "%Li" i
    | Nativeint -> Format.fprintf ppf "%ni" i

and print_approx ppf { descr } = print_descr ppf descr

(** Smart constructors *)

let approx descr = { descr; var = None; symbol = None }

let value_unknown = approx Value_unknown
let value_int i = approx (Value_int i)
let value_constptr i = approx (Value_constptr i)
let value_float f = approx (Value_float f)
let value_boxed_int bi i = approx (Value_boxed_int (bi,i))
let value_closure c = approx (Value_closure c)
let value_set_of_closures c = approx (Value_set_of_closures c)
let value_block (t,b) = approx (Value_block (t,b))
let value_extern ex = approx (Value_extern ex)
let value_symbol sym = { (approx (Value_symbol sym)) with symbol = Some sym }
let value_bottom = approx Value_bottom
let value_unresolved sym = approx (Value_unresolved sym)

let make_const_int n eid =
  Fconst(Fconst_base(Asttypes.Const_int n),eid), value_int n
let make_const_ptr n eid = Fconst(Fconst_pointer n,eid), value_constptr n
let make_const_bool b eid = make_const_ptr (if b then 1 else 0) eid
let make_const_float f eid = Fconst(Fconst_float f,eid), value_float f
let make_const_boxed_int (type bi) (t:bi boxed_int) (i:bi) eid =
  let c = match t with
    | Int32 -> Asttypes.Const_int32 i
    | Int64 -> Asttypes.Const_int64 i
    | Nativeint -> Asttypes.Const_nativeint i in
  Fconst(Fconst_base c,eid), value_boxed_int t i

let const_approx = function
  | Fconst_base const ->
      let open Asttypes in
      begin match const with
      | Const_int i -> value_int i
      | Const_char c -> value_int (Char.code c)
      | Const_string _ -> value_unknown
      | Const_float s -> value_float (float_of_string s)
      | Const_int32 i -> value_boxed_int Int32 i
      | Const_int64 i -> value_boxed_int Int64 i
      | Const_nativeint i -> value_boxed_int Nativeint i
      end
  | Fconst_pointer i -> value_constptr i
  | Fconst_float f -> value_float f
  | Fconst_float_array _ -> value_unknown
  | Fconst_immstring _ -> value_unknown

let check_constant_result (lam : 'a flambda) approx =
  if Flambdaeffects.no_effects lam then
    match approx.descr with
    | Value_int n ->
      make_const_int n (Flambdautils.data_at_toplevel_node lam)
    | Value_constptr n ->
      make_const_ptr n (Flambdautils.data_at_toplevel_node lam)
    | Value_float f ->
      make_const_float f (Flambdautils.data_at_toplevel_node lam)
    | Value_boxed_int (t,i) ->
      make_const_boxed_int t i (Flambdautils.data_at_toplevel_node lam)
    | Value_symbol sym ->
      Fsymbol(sym, Flambdautils.data_at_toplevel_node lam), approx
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown | Value_bottom | Value_extern _ | Value_unresolved _ ->
      lam, approx
  else
    lam, approx

let check_var_and_constant_result ~is_present_in_env lam approx =
  let res = match approx.var with
    | Some var when is_present_in_env var ->
        Fvar(var, Flambdautils.data_at_toplevel_node lam)
    | _ ->
        match approx.symbol with
        | Some sym ->
            Fsymbol(sym, Flambdautils.data_at_toplevel_node lam)
        | None -> lam
  in
  check_constant_result res approx

let known t =
  match t.descr with
  | Value_unresolved _
  | Value_unknown -> false
  | Value_bottom | Value_block _ | Value_int _ | Value_constptr _
  | Value_set_of_closures _ | Value_closure _ | Value_extern _
  | Value_float _ | Value_boxed_int _ | Value_symbol _ -> true

let useful t =
  match t.descr with
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_block _ | Value_int _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ | Value_extern _
  | Value_symbol _ -> true

let is_certainly_immutable t =
  match t.descr with
  | Value_block _ | Value_int _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ -> true
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_extern _ | Value_symbol _ -> assert false

let get_field i = function
  | [] | _ :: _ :: _ -> assert false
  | [{descr}] ->
    match descr with
    | Value_block (tag, fields) ->
      if i >= 0 && i < Array.length fields
      then fields.(i)
      else value_unknown
    | Value_int _ | Value_constptr _ | Value_float _ | Value_boxed_int _
    | Value_set_of_closures _ | Value_closure _
    | Value_unknown | Value_bottom
    | Value_symbol _ | Value_extern _ ->
      value_unknown
    | Value_unresolved sym ->
      value_unresolved sym

let descrs approxs = List.map (fun v -> v.descr) approxs

let equal_boxed_int (type t1) (type t2)
    (bi1:t1 boxed_int) (i1:t1)
    (bi2:t2 boxed_int) (i2:t2) =
  match bi1, bi2 with
  | Int32, Int32 -> Int32.equal i1 i2
  | Int64, Int64 -> Int64.equal i1 i2
  | Nativeint, Nativeint -> Nativeint.equal i1 i2
  | _ -> false

(* Closures and set of closures descriptions cannot be merged.

   let f x =
     let g y -> x + y in
     g
   in
   let v =
     if ...
     then f 1
     else f 2
   in
   v 3

   The approximation for [f 1] and [f 2] could both contain the
   description of [g]. But if [f] where inlined, a new [g] would
   be created in each branch, leading to incompatible description.
   And we must never make the descrition for a function less
   precise that it used to be: its information are needed for
   rewriting [Fvariable_in_closure] and [Fclosure] constructions
   in [Flambdainline.loop]
*)
let rec meet_descr d1 d2 = match d1, d2 with
  | Value_int i, Value_int j when i = j ->
      d1
  | Value_constptr i, Value_constptr j when i = j ->
      d1
  | Value_symbol s1, Value_symbol s2 when Symbol.equal s1 s2 ->
      d1
  | Value_extern e1, Value_extern e2 when Flambdaexport.ExportId.equal e1 e2 ->
      d1
  | Value_float i, Value_float j when i = j ->
      d1
  | Value_boxed_int (bi1, i1), Value_boxed_int (bi2, i2) when
      equal_boxed_int bi1 i1 bi2 i2 ->
      d1
  | Value_block (tag1, a1), Value_block (tag2, a2)
    when tag1 = tag2 && Array.length a1 = Array.length a2 ->
      Value_block (tag1, Array.mapi (fun i v -> meet v a2.(i)) a1)
  | _ -> Value_unknown

and meet a1 a2 =
  match a1, a2 with
  | { descr = Value_bottom }, a
  | a, { descr = Value_bottom } -> a
  | _ ->
      let var =
        match a1.var, a2.var with
        | None, _ | _, None -> None
        | Some v1, Some v2 ->
            if Variable.equal v1 v2
            then Some v1
            else None
      in
      let symbol =
        match a1.symbol, a2.symbol with
        | None, _ | _, None -> None
        | Some v1, Some v2 ->
            if Symbol.equal v1 v2
            then Some v1
            else None
      in
      { descr = meet_descr a1.descr a2.descr;
        var;
        symbol }

(** Import external approx *)

module Import = struct
  open Flambdaexport

  let reported_missing_symbols = SymbolTbl.create 0

  let rec import_ex ex : t =

    ignore(Compilenv.approx_for_global (ExportId.unit ex));

    let ex_info = Compilenv.approx_env () in
    try match find_description ex ex_info with
      | Value_int i -> value_int i
      | Value_constptr i -> value_constptr i
      | Value_float f -> value_float f
      | Value_boxed_int (t,i) -> value_boxed_int t i
      | Value_string -> value_unknown
      | Value_block (tag, fields) ->
          value_block (tag, Array.map import_approx fields)
      | Value_closure { fun_id; closure = { closure_id; bound_var } } ->
        let bound_var = Var_within_closure.Map.map import_approx bound_var in
        let unchanging_params =
          try Set_of_closures_id.Map.find closure_id ex_info.ex_kept_arguments with
          | Not_found -> assert false
        in
        value_closure
          { fun_id;
            set_of_closures_var = None;
            set_of_closures =
              { ffunctions = Compilenv.imported_closure closure_id;
                bound_var;
                unchanging_params = unchanging_params;
                specialised_args = Variable.Set.empty;
                ffunction_sb =
                  Flambdasubst.
                  Alpha_renaming_map_for_ids_and_bound_vars_of_closures.empty;
              } }
      | Value_set_of_closures { closure_id; bound_var } ->
        let bound_var = Var_within_closure.Map.map import_approx bound_var in
        let unchanging_params =
          try Set_of_closures_id.Map.find closure_id ex_info.ex_kept_arguments with
          | Not_found -> assert false
        in
        value_set_of_closures
          { ffunctions = Compilenv.imported_closure closure_id;
            bound_var;
            unchanging_params = unchanging_params;
            specialised_args = Variable.Set.empty;
            ffunction_sb =
              Flambdasubst.
              Alpha_renaming_map_for_ids_and_bound_vars_of_closures.empty; }
    with Not_found ->
      value_unknown

  and import_approx (ap:Flambdaexport.approx) : t =
    match ap with
    | Value_unknown -> value_unknown
    | Value_id ex -> value_extern ex
    | Value_symbol sym -> value_symbol sym

  let import_symbol sym : t =
    if Compilenv.is_predefined_exception sym then
      value_unknown
    else
      let symbol_id_map =
        (Compilenv.approx_for_global sym.sym_unit).ex_symbol_id in
      try import_ex (SymbolMap.find sym symbol_id_map) with
      | Not_found ->
        if not (SymbolTbl.mem reported_missing_symbols sym)
        then begin
          SymbolTbl.add reported_missing_symbols sym ();
          Location.prerr_warning (Location.in_file "some_file")
            (Warnings.Missing_symbol_information
               (Format.asprintf "%a" Symbol.print sym,
                Format.asprintf "%a" Compilation_unit.print sym.Symbol.sym_unit));
        end;
        value_unresolved sym

  let rec really_import = function
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
end

let really_import_approx approx =
  { approx with descr = Import.really_import approx.descr }

let which_function_parameters_can_we_specialize ~params ~args
      ~approximations_of_args ~unchanging_params =
  assert (List.length params = List.length args);
  assert (List.length args = List.length approximations_of_args);
  List.fold_right2 (fun (id, arg) approx (spec_args, args, args_decl) ->
      let new_id, args_decl =
        (* If the argument expression is not a variable, we declare a new one.
           This is needed for adding arguments to cl_specialised_arg which
           requires a variable *)
        match arg with
        | Fvar (var,_) ->
            var, args_decl
        | _ ->
            let new_id = Flambdasubst.freshen_var id in
            let args_decl = (new_id, arg) :: args_decl in
            new_id, args_decl in
      let spec_args =
        if useful approx && Variable.Set.mem id unchanging_params then
          Variable.Map.add id new_id spec_args
        else
          spec_args
      in
      spec_args, new_id :: args, args_decl)
    (List.combine params args) approximations_of_args
    (Variable.Map.empty, [], [])
