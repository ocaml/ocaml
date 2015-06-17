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

module Tag = struct
  type t = int

  let create_exn tag =
    if tag < 0 || tag > 255 then
      Misc.fatal_error (Printf.sprintf "Tag.create_exn %d" tag)
    else
      tag

  let to_int t = t
end

type 'a boxed_int = 'a Flambdaexport.boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of Flambdaexport.value_string
  | Value_float_array of int (* size *)
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t
  | Value_unresolved of Symbol.t

and value_closure =
  { closure_id : Closure_id.t;
    set_of_closures : value_set_of_closures;
    set_of_closures_var : Variable.t option;
  }

and value_set_of_closures =
  { function_decls : Expr_id.t Flambda.function_declarations;
    bound_var : t Var_within_closure.Map.t;
    unchanging_params : Variable.Set.t;
    specialised_args : Variable.Set.t;
    alpha_renaming :
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
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" (Tag.to_int tag) p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Flambdaexport.ExportId.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { closure_id } ->
    Format.fprintf ppf "(fun:@ %a)" Closure_id.print closure_id
  | Value_set_of_closures { function_decls = { funs } } ->
    Format.fprintf ppf "(set_of_closures:@ %a)"
      (fun ppf -> Variable.Map.iter (fun id _ -> Variable.print ppf id)) funs
  | Value_unresolved sym ->
    Format.fprintf ppf "(unresolved %a)" Symbol.print sym
  | Value_float f -> Format.pp_print_float ppf f
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
  | Value_float_array size ->
      Format.fprintf ppf "float_array %i" size
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

let value_string size contents = approx (Value_string {size; contents })
let value_float_array size = approx (Value_float_array size)

let make_const_int n eid : _ Flambda.t * t =
  Fconst(Fconst_base(Asttypes.Const_int n),eid), value_int n

let make_const_ptr n eid : _ Flambda.t * t =
  Fconst(Fconst_pointer n,eid), value_constptr n

let make_const_bool b eid : _ Flambda.t * t =
  make_const_ptr (if b then 1 else 0) eid

let make_const_float f eid : _ Flambda.t * t =
  Fconst(Fconst_float f,eid), value_float f

let make_const_boxed_int (type bi) (t:bi boxed_int) (i:bi) eid
      : _ Flambda.t * t =
  let c : Asttypes.constant =
    match t with
    | Int32 -> Const_int32 i
    | Int64 -> Const_int64 i
    | Nativeint -> Const_nativeint i
  in
  Fconst (Fconst_base c, eid), value_boxed_int t i

let const_approx (flam : Flambda.const) =
  match flam with
  | Fconst_base const ->
    begin match const with
    | Const_int i -> value_int i
    | Const_char c -> value_int (Char.code c)
    | Const_string (s,_) -> value_string (String.length s) None
    | Const_float s -> value_float (float_of_string s)
    | Const_int32 i -> value_boxed_int Int32 i
    | Const_int64 i -> value_boxed_int Int64 i
    | Const_nativeint i -> value_boxed_int Nativeint i
    end
  | Fconst_pointer i -> value_constptr i
  | Fconst_float f -> value_float f
  | Fconst_float_array a -> value_float_array (List.length a)
  | Fconst_immstring s -> value_string (String.length s) (Some s)

let check_constant_result (lam : _ Flambda.t) approx =
  if Effect_analysis.no_effects lam then
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
      Fsymbol (sym, Flambdautils.data_at_toplevel_node lam), approx
    | Value_string _ | Value_float_array _
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown | Value_bottom | Value_extern _ | Value_unresolved _ ->
      lam, approx
  else
    lam, approx

let check_var_and_constant_result ~is_present_in_env lam approx =
  let res : _ Flambda.t =
    match approx.var with
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
  | Value_string _ | Value_float_array _
  | Value_bottom | Value_block _ | Value_int _ | Value_constptr _
  | Value_set_of_closures _ | Value_closure _ | Value_extern _
  | Value_float _ | Value_boxed_int _ | Value_symbol _ -> true

let useful t =
  match t.descr with
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_string _ | Value_float_array _
  | Value_block _ | Value_int _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ | Value_extern _
  | Value_symbol _ -> true

let is_certainly_immutable t =
  match t.descr with
  | Value_string { contents = Some _ }
  | Value_block _ | Value_int _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ -> true
  | Value_string { contents = None } | Value_float_array _
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_extern _ | Value_symbol _ -> assert false

let get_field i = function
  | [] | _ :: _ :: _ -> assert false
  | [{descr}] ->
    match descr with
    | Value_block (_tag, fields) ->
      if i >= 0 && i < Array.length fields
      then fields.(i)
      else value_unknown
    | Value_bottom
    | Value_int _ | Value_constptr _ ->
        (* Something seriously wrong is happening: either the user is doing something
           exceptionnaly unsafe, or it is an unreachable branch:
           We consider this is unreachable and mark the result as it *)
        value_bottom
    | Value_float_array _ ->
        (* float_arrays are immutable *)
        value_unknown
    | Value_string _ | Value_float _ | Value_boxed_int _  (* The user is doing something unsafe *)
    | Value_set_of_closures _ | Value_closure _
    (* This is used by CamlinternalMod... *)
    | Value_symbol _ | Value_extern _
      (* Should have been resolved *)
    | Value_unknown ->
        value_unknown
    | Value_unresolved sym ->
        (* We don't know anything, but we must remember that it comes
           from another compilation unit in case it contained a closure *)
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
   rewriting [Fvar_within_closure] and [Fselect_closure] constructions
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
  module SymbolTbl = Symbol.SymbolTbl
  module SymbolMap = Symbol.SymbolMap

  let reported_missing_symbols = SymbolTbl.create 0

  let rec import_ex ex : t =
    let (_ : Flambdaexport.exported) =
      Compilenv.approx_for_global (Flambdaexport.ExportId.unit ex)
    in
    let ex_info = Compilenv.approx_env () in
    try match Flambdaexport.find_description ex ex_info with
      | Value_int i -> value_int i
      | Value_constptr i -> value_constptr i
      | Value_float f -> value_float f
      | Value_float_array size -> value_float_array size
      | Flambdaexport.Value_boxed_int (t,i) -> value_boxed_int t i
      | Value_string { size; contents } -> value_string size contents
      | Value_mutable_block _ -> value_unknown
      | Value_block (tag, fields) ->
          value_block (tag, Array.map import_approx fields)
      | Value_closure { fun_id; closure = { closure_id; bound_var } } ->
        let bound_var = Var_within_closure.Map.map import_approx bound_var in
        let unchanging_params =
          try Set_of_closures_id.Map.find closure_id ex_info.ex_kept_arguments with
          | Not_found -> assert false
        in
        value_closure
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
        value_set_of_closures
          { function_decls = Compilenv.imported_closure closure_id;
            bound_var;
            unchanging_params = unchanging_params;
            specialised_args = Variable.Set.empty;
            alpha_renaming =
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
      match import_ex (SymbolMap.find sym symbol_id_map) with
      | approx -> { approx with symbol = Some sym }
      | exception Not_found ->
        if not (SymbolTbl.mem reported_missing_symbols sym)
        then begin
          SymbolTbl.add reported_missing_symbols sym ();
          Location.prerr_warning (Location.in_file "some_file")
            (Warnings.Missing_symbol_information
               (Format.asprintf "%a" Symbol.print sym,
                Format.asprintf "%a" Symbol.Compilation_unit.print
                  sym.Symbol.sym_unit));
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
