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

module U = Flambda_utils

type 'a boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type value_string = {
  (* CR mshinwell: use variant *)
  contents : string option; (* None if unknown or mutable *)
  size : int;
}

type t = {
  descr : descr;
  var : Variable.t option;
  symbol : (Symbol.t * int option) option;
}

and descr =
  | Value_block of Tag.t * t array
  | Value_int of int
  | Value_char of char
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_closure
  | Value_string of value_string
  | Value_float_array of int (* size *)
  | Value_unknown
  | Value_bottom
  | Value_extern of Export_id.t
  (* CR mshinwell: Why does Value_symbol need to be here?
     Related to not forcing resolution of everything all at once.
     Why does Value_unresolved need an argument? We have [symbol] *)
  | Value_symbol of Symbol.t
  | Value_unresolved of Symbol.t (* No description was found for this symbol *)

and value_closure = {
  set_of_closures : t;
  closure_id : Closure_id.t;
}

and value_set_of_closures = {
  function_decls : Flambda.function_declarations;
  bound_vars : t Var_within_closure.Map.t;
  invariant_params : Variable.Set.t;
  specialised_args : Variable.Set.t;
  freshening : Freshening.Project_var.t;
}

let descr t = t.descr

let print_value_set_of_closures ppf
      { function_decls = { funs }; invariant_params; _ } =
  Format.fprintf ppf "(set_of_closures:@ %a invariant_params=%a)"
    (fun ppf -> Variable.Map.iter (fun id _ -> Variable.print ppf id)) funs
    Variable.Set.print invariant_params

let rec print_descr ppf = function
  | Value_int i -> Format.pp_print_int ppf i
  | Value_char c -> Format.fprintf ppf "%c" c
  | Value_constptr i -> Format.fprintf ppf "%ia" i
  | Value_block (tag,fields) ->
    let p ppf fields =
      Array.iter (fun v -> Format.fprintf ppf "%a@ " print v) fields in
    Format.fprintf ppf "[%i:@ @[<1>%a@]]" (Tag.to_int tag) p fields
  | Value_unknown -> Format.fprintf ppf "?"
  | Value_bottom -> Format.fprintf ppf "bottom"
  | Value_extern id -> Format.fprintf ppf "_%a_" Export_id.print id
  | Value_symbol sym -> Format.fprintf ppf "%a" Symbol.print sym
  | Value_closure { set_of_closures; closure_id; } ->
    Format.fprintf ppf "(closure:@ %a from@ %a)" Closure_id.print closure_id
      print set_of_closures
  | Value_set_of_closures set_of_closures ->
    print_value_set_of_closures ppf set_of_closures
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

and print ppf { descr; var; symbol; } =
  let print ppf = function
    | None -> Symbol.print_opt ppf None
    | Some (sym, None) -> Symbol.print ppf sym
    | Some (sym, Some field) ->
        Format.fprintf ppf "%a.(%i)" Symbol.print sym field
  in
  Format.fprintf ppf "{ descr=%a var=%a symbol=%a }"
    print_descr descr
    Variable.print_opt var
    print symbol

let approx descr = { descr; var = None; symbol = None }

let augment_with_variable t var = { t with var = Some var }
let augment_with_symbol t symbol = { t with symbol = Some (symbol, None) }
let augment_with_symbol_field t symbol field =
  match t.symbol with
  | None -> { t with symbol = Some (symbol, Some field) }
  | Some _ -> t
let replace_description t descr = { t with descr }

let value_unknown = approx Value_unknown
let value_int i = approx (Value_int i)
let value_char i = approx (Value_char i)
let value_constptr i = approx (Value_constptr i)
let value_float f = approx (Value_float f)
let value_boxed_int bi i = approx (Value_boxed_int (bi,i))

let value_closure ?closure_var ?set_of_closures_var ?set_of_closures_symbol
      value_set_of_closures closure_id =
  let approx_set_of_closures =
    { descr = Value_set_of_closures value_set_of_closures;
      var = set_of_closures_var;
      symbol = Misc.may_map (fun s -> s, None) set_of_closures_symbol;
    }
  in
  let value_closure =
    { set_of_closures = approx_set_of_closures;
      closure_id;
    }
  in
  { descr = Value_closure value_closure;
    var = closure_var;
    symbol = None;
  }

let value_set_of_closures ?set_of_closures_var value_set_of_closures =
  { descr = Value_set_of_closures value_set_of_closures;
    var = set_of_closures_var;
    symbol = None;
  }

let value_block (t,b) = approx (Value_block (t,b))
let value_extern ex = approx (Value_extern ex)
let value_symbol sym = { (approx (Value_symbol sym)) with symbol = Some (sym, None) }
let value_bottom = approx Value_bottom
let value_unresolved sym = approx (Value_unresolved sym)

let value_string size contents = approx (Value_string {size; contents })
let value_float_array size = approx (Value_float_array size)

let name_expr_fst (named, thing) = (Flambda_utils.name_expr named), thing

let make_const_int_named n : Flambda.named * t =
  Const (Int n), value_int n
let make_const_int n = name_expr_fst (make_const_int_named n)

let make_const_char_named n : Flambda.named * t =
  Const (Char n), value_char n
let make_const_char n = name_expr_fst (make_const_char_named n)

let make_const_ptr_named n : Flambda.named * t =
  Const (Const_pointer n), value_constptr n
let make_const_ptr n = name_expr_fst (make_const_ptr_named n)

let make_const_bool_named b : Flambda.named * t =
  make_const_ptr_named (if b then 1 else 0)
let make_const_bool b = name_expr_fst (make_const_bool_named b)

let make_const_float_named f : Flambda.named * t =
  Allocated_const (Float f), value_float f
let make_const_float f = name_expr_fst (make_const_float_named f)

let make_const_boxed_int_named (type bi) (t:bi boxed_int) (i:bi)
      : Flambda.named * t =
  let c : Allocated_const.t =
    match t with
    | Int32 -> Int32 i
    | Int64 -> Int64 i
    | Nativeint -> Nativeint i
  in
  Allocated_const c, value_boxed_int t i
let make_const_boxed_int t i = name_expr_fst (make_const_boxed_int_named t i)

type simplification_summary =
  | Nothing_done
  | Replaced_term

type simplification_result = Flambda.t * simplification_summary * t
type simplification_result_named = Flambda.named * simplification_summary * t

let simplify t (lam : Flambda.t) : simplification_result =
  if Effect_analysis.no_effects lam then
    match t.descr with
    | Value_int n ->
      let const, approx = make_const_int n in
      const, Replaced_term, approx
    | Value_char n ->
      let const, approx = make_const_char n in
      const, Replaced_term, approx
    | Value_constptr n ->
      let const, approx = make_const_ptr n in
      const, Replaced_term, approx
    | Value_float f ->
      let const, approx = make_const_float f in
      const, Replaced_term, approx
    | Value_boxed_int (t, i) ->
      let const, approx = make_const_boxed_int t i in
      const, Replaced_term, approx
    | Value_symbol sym ->
      U.name_expr (Symbol sym), Replaced_term, t
    | Value_string _ | Value_float_array _
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown | Value_bottom | Value_extern _ | Value_unresolved _ ->
      lam, Nothing_done, t
  else
    lam, Nothing_done, t

let simplify_named t (named : Flambda.named) : simplification_result_named =
  if Effect_analysis.no_effects_named named then
    match t.descr with
    | Value_int n ->
      let const, approx = make_const_int_named n in
      const, Replaced_term, approx
    | Value_char n ->
      let const, approx = make_const_char_named n in
      const, Replaced_term, approx
    | Value_constptr n ->
      let const, approx = make_const_ptr_named n in
      const, Replaced_term, approx
    | Value_float f ->
      let const, approx = make_const_float_named f in
      const, Replaced_term, approx
    | Value_boxed_int (t, i) ->
      let const, approx = make_const_boxed_int_named t i in
      const, Replaced_term, approx
    | Value_symbol sym ->
      Symbol sym, Replaced_term, t
    | Value_string _ | Value_float_array _
    | Value_block _ | Value_set_of_closures _ | Value_closure _
    | Value_unknown | Value_bottom | Value_extern _ | Value_unresolved _ ->
      named, Nothing_done, t
  else
    named, Nothing_done, t

(* CR mshinwell: bad name *)
let simplify_var t : (Flambda.named * t) option =
  match t.descr with
  | Value_int n -> Some (make_const_int_named n)
  | Value_char n -> Some (make_const_char_named n)
  | Value_constptr n -> Some (make_const_ptr_named n)
  | Value_float f -> Some (make_const_float_named f)
  | Value_boxed_int (t, i) -> Some (make_const_boxed_int_named t i)
  | Value_symbol sym -> Some (Symbol sym, t)
  | Value_string _ | Value_float_array _
  | Value_block _ | Value_set_of_closures _ | Value_closure _
  | Value_unknown | Value_bottom | Value_extern _ | Value_unresolved _ -> None

let join_summaries summary ~replaced_by_var_or_symbol =
  match replaced_by_var_or_symbol, summary with
  | true, Nothing_done
  | true, Replaced_term
  | false, Replaced_term -> Replaced_term
  | false, Nothing_done -> Nothing_done

let simplify_using_env t ~is_present_in_env flam =
  let replaced_by_var_or_symbol, flam =
    match t.var with
    | Some var when is_present_in_env var -> true, Flambda.Var var
    | _ ->
      match t.symbol with
      | Some (sym, None) -> true, U.name_expr (Symbol sym)
      | Some (sym, Some field) ->
          true, U.name_expr (Read_symbol_field (sym, field))
      | None -> false, flam
  in
  let const, summary, approx = simplify t flam in
  const, join_summaries summary ~replaced_by_var_or_symbol, approx

let simplify_named_using_env t ~is_present_in_env named =
  let replaced_by_var_or_symbol, named =
    match t.var with
    | Some var when is_present_in_env var ->
      true, Flambda.Expr (Var var)
    | _ ->
      match t.symbol with
      | Some (sym, None) -> true, (Flambda.Symbol sym:Flambda.named)
      | Some (sym, Some field) ->
          true, Flambda.Read_symbol_field (sym, field)
      | None -> false, named
  in
  let const, summary, approx = simplify_named t named in
  const, join_summaries summary ~replaced_by_var_or_symbol, approx

let simplify_var_to_var_using_env t ~is_present_in_env =
  match t.var with
  | Some var when is_present_in_env var -> Some var
  | _ -> None

let known t =
  match t.descr with
  | Value_unresolved _
  | Value_unknown -> false
  | Value_string _ | Value_float_array _
  | Value_bottom | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_set_of_closures _ | Value_closure _
  | Value_extern _ | Value_float _ | Value_boxed_int _ | Value_symbol _ -> true

let useful t =
  match t.descr with
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_string _ | Value_float_array _ | Value_block _ | Value_int _
  | Value_char _ | Value_constptr _ | Value_set_of_closures _
  | Value_float _ | Value_boxed_int _ | Value_closure _ | Value_extern _
  | Value_symbol _ -> true

let is_definitely_immutable t =
  match t.descr with
  | Value_string { contents = Some _ }
  | Value_block _ | Value_int _ | Value_char _ | Value_constptr _
  | Value_set_of_closures _ | Value_float _ | Value_boxed_int _
  | Value_closure _ -> true
  | Value_string { contents = None } | Value_float_array _
  | Value_unresolved _ | Value_unknown | Value_bottom -> false
  | Value_extern _ | Value_symbol _ -> assert false

let get_field t ~field_index:i =
  match t.descr with
  | Value_block (_tag, fields) ->
    if i >= 0 && i < Array.length fields then
      fields.(i)
    else
      Misc.fatal_errorf "Simple_value_approx.get_field: attempt to project \
          block approximation at field %d, but it only has %d fields.  \
          Approximation: %a"
        i (Array.length fields)
        print t
  | Value_bottom
  | Value_int _ | Value_char _ | Value_constptr _ ->
    (* Something seriously wrong is happening: either the user is doing
       something exceptionally unsafe, or it is an unreachable branch.
       We consider this as unreachable and mark the result accordingly. *)
    value_bottom
  | Value_float_array _ ->
    (* CR mshinwell: comment needs improvement *)
    (* float_arrays are immutable *)
    value_unknown
  | Value_string _ | Value_float _ | Value_boxed_int _
    (* The user is doing something unsafe. *)
  | Value_set_of_closures _ | Value_closure _
    (* This is used by [CamlinternalMod]. *)
  | Value_symbol _ | Value_extern _
    (* These should have been resolved. *)
  | Value_unknown ->
    value_unknown
  | Value_unresolved sym ->
    (* We don't know anything, but we must remember that it comes
       from another compilation unit in case it contains a closure. *)
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
   rewriting [Project_var] and [Project_closure] constructions
   in [Flambdainline.loop]
*)
let rec meet_descr d1 d2 = match d1, d2 with
  | Value_int i, Value_int j when i = j ->
      d1
  | Value_constptr i, Value_constptr j when i = j ->
      d1
  | Value_symbol s1, Value_symbol s2 when Symbol.equal s1 s2 ->
      d1
  | Value_extern e1, Value_extern e2 when Export_id.equal e1 e2 ->
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
        | Some (v1, field1), Some (v2, field2) ->
            if Symbol.equal v1 v2
            then match field1, field2 with
              | None, None -> a1.symbol
              | Some f1, Some f2 when f1 = f2 ->
                  a1.symbol
              | _ -> None
            else None
      in
      { descr = meet_descr a1.descr a2.descr;
        var;
        symbol }

(* Given a set-of-closures approximation and a closure ID, apply any
   freshening specified in the approximation to the closure ID, and return
   that new closure ID.  A fatal error is produced if the new closure ID
   does not correspond to a function declaration in the given approximation. *)
let freshen_and_check_closure_id
      (value_set_of_closures : value_set_of_closures) closure_id =
  let closure_id =
    Freshening.Project_var.apply_closure_id
      value_set_of_closures.freshening closure_id
  in
  try
    ignore (Flambda_utils.find_declaration closure_id
      value_set_of_closures.function_decls);
    closure_id
  with Not_found ->
    Misc.fatal_error (Format.asprintf
      "Function %a not found in the set of closures@ %a@.%a@."
      Closure_id.print closure_id
      print_value_set_of_closures value_set_of_closures
      Flambda.print_function_declarations value_set_of_closures.function_decls)

type checked_approx_for_set_of_closures =
  | Wrong
  | Unresolved of Symbol.t
  | Ok of Variable.t option * value_set_of_closures

let check_approx_for_set_of_closures t : checked_approx_for_set_of_closures =
  match t.descr with
  | Value_unresolved symbol ->
    (* CR mshinwell: is it possible to check that this value really does
       come from another compilation unit? *)
    Unresolved symbol
  | Value_set_of_closures value_set_of_closures ->
    (* Note that [var] might be [None]; we might be reaching the set of
       closures via approximations only, with the variable originally bound
       to the set now out of scope. *)
    Ok (t.var, value_set_of_closures)
  | Value_closure _ | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_float _ | Value_boxed_int _ | Value_unknown
  | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
  | Value_symbol _ ->
    Wrong

type checked_approx_for_closure_allowing_unresolved =
  | Wrong
  | Unresolved of Symbol.t
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

let check_approx_for_closure_allowing_unresolved t
      : checked_approx_for_closure_allowing_unresolved =
  match t.descr with
  | Value_closure value_closure ->
    (* CR mshinwell: not exactly sure yet what to allow here *)
    begin match value_closure.set_of_closures.descr with
    | Value_set_of_closures value_set_of_closures ->
      let symbol = match value_closure.set_of_closures.symbol with
        | Some (symbol, None) -> Some symbol
        | None | Some (_, Some _) -> None
      in
      Ok (value_closure, value_closure.set_of_closures.var,
          symbol, value_set_of_closures)
    | Value_unresolved _
    | Value_closure _ | Value_block _ | Value_int _ | Value_char _
    | Value_constptr _ | Value_float _ | Value_boxed_int _ | Value_unknown
    | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
    | Value_symbol _ ->
      Wrong
    end
  | Value_unresolved symbol -> Unresolved symbol
  | Value_set_of_closures _ | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_float _ | Value_boxed_int _ | Value_unknown
  | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
  | Value_symbol _ ->
    Wrong

type checked_approx_for_set_of_closures_allowing_unknown_and_unresolved =
  | Wrong
  | Unknown_or_unresolved
  | Ok of value_set_of_closures

let checked_approx_for_set_of_closures_allowing_unknown_and_unresolved t
    : checked_approx_for_set_of_closures_allowing_unknown_and_unresolved =
  match t.descr with
  | Value_set_of_closures value_set_closures ->
    Ok value_set_closures
  | Value_unknown
  | Value_unresolved _ ->
    Unknown_or_unresolved
  | Value_closure _ | Value_block _ | Value_int _ | Value_char _
  | Value_constptr _ | Value_float _ | Value_boxed_int _
  | Value_bottom | Value_extern _ | Value_string _ | Value_float_array _
  | Value_symbol _ ->
    Wrong

type checked_approx_for_closure =
  | Wrong
  | Ok of value_closure * Variable.t option
          * Symbol.t option * value_set_of_closures

let check_approx_for_closure t : checked_approx_for_closure =
  match check_approx_for_closure_allowing_unresolved t with
  | Ok (value_closure, set_of_closures_var, set_of_closures_symbol, value_set_of_closures) ->
    Ok (value_closure, set_of_closures_var, set_of_closures_symbol, value_set_of_closures)
  | Wrong | Unresolved _ -> Wrong

let approx_for_bound_var value_set_of_closures var =
  try
    Var_within_closure.Map.find var value_set_of_closures.bound_vars
  with
  | Not_found ->
    Misc.fatal_errorf "The set-of-closures approximation %a@ does not \
        bind the variable %a@.%s@."
      print_value_set_of_closures value_set_of_closures
      Var_within_closure.print var
      (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int))
