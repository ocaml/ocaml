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

open Abstract_identifiers
open Flambda

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnative : nativeint -> nativeint = "%bswap_native"

let lift_lets tree =
  let rec aux = function
    | Flet(str1,v1,Flet(str2,v2,def2,body2,d2),body1,d1) ->
        Flet(str2,v2,def2,
             aux (Flet(str1,v1,body2,body1,d1)),d2)
    | e -> e in
  Flambdaiter.map aux tree

(** An variable in a closure can either be used by the closure itself
    or by an inlined version of the function. *)
let remove_unused_closure_variables tree =
  let used_variable_within_closure =
    let used = ref Var_within_closure.Set.empty in
    let aux expr = match expr with
      | Fvariable_in_closure({ vc_var },_) ->
         used := Var_within_closure.Set.add vc_var !used
      | e -> ()
    in
    Flambdaiter.iter aux tree;
    !used
  in
  let aux = function
    | Fset_of_closures ({ cl_fun; cl_free_var } as closure, eid) ->
       let all_free_var =
         Variable.Map.fold
           (fun _ { free_variables } acc ->
             Variable.Set.union free_variables acc)
           cl_fun.funs
           Variable.Set.empty in
       let cl_free_var =
         Variable.Map.filter (fun id _ ->
           Variable.Set.mem id all_free_var
             || Var_within_closure.Set.mem (Var_within_closure.wrap id)
                                       used_variable_within_closure)
           cl_free_var in
       Fset_of_closures ({ closure with cl_free_var }, eid)
    | e -> e in
  Flambdaiter.map aux tree

(* CR mshinwell: rename [eid] and/or [annot] to be consistent *)
let const_int_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_int n eid
  else expr, Flambdaapprox.value_int n
let const_ptr_expr expr n eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_ptr n eid
  else expr, Flambdaapprox.value_constptr n
let const_bool_expr expr b eid =
  const_ptr_expr expr (if b then 1 else 0) eid
let const_float_expr expr f eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_float f eid
  else expr, Flambdaapprox.value_float f
let const_boxed_int_expr expr t i eid =
  if Flambdaeffects.no_effects expr
  then Flambdaapprox.make_const_boxed_int t i eid
  else expr, Flambdaapprox.value_boxed_int t i

let const_comparison_expr expr cmp x y eid =
  let open Lambda in
  const_bool_expr expr
    (match cmp with
     | Ceq -> x = y
     | Cneq -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)
    eid

module Simplify_sequential_logical_operator (G : sig
  val canonical_absorbing_element : int
  val is_absorbing_element : int -> bool
  val primitive : Lambda.primitive
end) = struct
  let sequential_op ~arg1 ~arg1_approx ~arg2 ~arg2_approx ~dbg ~annot =
    let arg1_no_effects = Flambdaeffects.no_effects arg1 in
    let arg2_no_effects = Flambdaeffects.no_effects arg2 in
    let arg2_annot = Flambdautils.data_at_toplevel_node arg2 in
    let module C = Flambdacost in
    let open Flambdaapprox in
    let completely_eliminated () =
      Fconst (Fconst_pointer G.canonical_absorbing_element, annot),
        Flambdaapprox.value_constptr G.canonical_absorbing_element,
        C.remove_branch (C.remove_code arg1 (
          C.remove_code arg2 C.no_benefit))
    in
    match arg1_approx.Flambdaapprox.descr with
    | (Value_int n | Value_constptr n) when G.is_absorbing_element n ->
      if arg1_no_effects then
        completely_eliminated ()
      else
        arg1, arg1_approx, C.remove_branch (C.remove_code arg2 C.no_benefit)
    | (Value_int n | Value_constptr n) -> (* when not the absorbing element *)
      if arg1_no_effects then
        arg2, arg2_approx, C.remove_branch (C.remove_code arg1 C.no_benefit)
      else
        begin match arg2_approx.Flambdaapprox.descr with
        | (Value_int arg2_val | Value_constptr arg2_val)
            when arg2_no_effects ->
          Fsequence (arg1, Fconst (Fconst_pointer arg2_val, arg2_annot),
              annot), arg2_approx,
            C.remove_branch (C.remove_code arg2 C.no_benefit)
        | _ ->
          Fsequence (arg1, arg2, annot), arg2_approx,
            C.remove_branch C.no_benefit
        end
    | _ ->
      match arg2_approx.Flambdaapprox.descr with
      | (Value_int n | Value_constptr n)
          when G.is_absorbing_element n ->
        begin match arg1_no_effects, arg2_no_effects with
        | true, true -> completely_eliminated ()
        | true, false (* we must run [arg1]: it might short-circuit [arg2] *)
        | false, false ->
          Fprim (G.primitive, [arg1; arg2], dbg, annot),
            Flambdaapprox.value_constptr G.canonical_absorbing_element,
              C.no_benefit
        | false, true ->
          Fsequence (arg1,
              Fconst (Fconst_pointer G.canonical_absorbing_element,
                arg2_annot), annot),
            Flambdaapprox.value_constptr G.canonical_absorbing_element,
              C.remove_branch (C.remove_code arg2 C.no_benefit)
        end
      | _ ->
        Fprim (G.primitive, [arg1; arg2], dbg, annot),
          Flambdaapprox.value_unknown, C.no_benefit
end

module Simplify_and = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 0
  let is_absorbing_element n = (n = 0)
  let primitive = Lambda.Psequand
end)
let sequential_and = Simplify_and.sequential_op

module Simplify_or = Simplify_sequential_logical_operator (struct
  let canonical_absorbing_element = 1
  let is_absorbing_element n = (n <> 0)
  let primitive = Lambda.Psequor
end)
let sequential_or = Simplify_or.sequential_op

let primitive p (args, approxs) expr : 'a flambda * Flambdaapprox.t =
  let open Lambda in
  let fpc = !Clflags.float_const_prop in
  match p with
  | Pmakeblock(tag, Asttypes.Immutable) ->
      expr, Flambdaapprox.value_block(tag, Array.of_list approxs)
  | _ ->
      let open Flambdaapprox in
      let eid = Flambdautils.data_at_toplevel_node expr in
      match descrs approxs with
      | [Value_int x] ->
          begin match p with
            Pidentity -> const_int_expr expr x eid
          | Pnot -> const_bool_expr expr (x = 0) eid
          | Pnegint -> const_int_expr expr (-x) eid
          | Pbswap16 -> const_int_expr expr (swap16 x) eid
          | Poffsetint y -> const_int_expr expr (x + y) eid
          | Pfloatofint when fpc -> const_float_expr expr (float_of_int x) eid
          | Pbintofint Pnativeint ->
            const_boxed_int_expr expr Nativeint (Nativeint.of_int x) eid
          | Pbintofint Pint32 ->
            const_boxed_int_expr expr Int32 (Int32.of_int x) eid
          | Pbintofint Pint64 ->
            const_boxed_int_expr expr Int64 (Int64.of_int x) eid
          | _ ->
              expr, value_unknown
          end
      | [(Value_int x | Value_constptr x);
         (Value_int y | Value_constptr y)] ->
          begin match p with
            Paddint -> const_int_expr expr (x + y) eid
          | Psubint -> const_int_expr expr (x - y) eid
          | Pmulint -> const_int_expr expr (x * y) eid
          | Pdivint when y <> 0 -> const_int_expr expr (x / y) eid
          | Pmodint when y <> 0 -> const_int_expr expr (x mod y) eid
          | Pandint -> const_int_expr expr (x land y) eid
          | Porint -> const_int_expr expr (x lor y) eid
          | Pxorint -> const_int_expr expr (x lxor y) eid
          | Plslint when 0 <= y && y < 8 * Arch.size_int ->
              const_int_expr expr (x lsl y) eid
          | Plsrint when 0 <= y && y < 8 * Arch.size_int ->
              const_int_expr expr (x lsr y) eid
          | Pasrint when 0 <= y && y < 8 * Arch.size_int ->
              const_int_expr expr (x asr y) eid
          | Pintcomp cmp ->
              const_comparison_expr expr cmp x y eid
          | Pisout ->
              const_bool_expr expr (y > x || y < 0) eid
          (* [Psequand] and [Psequor] are omitted, just in case one of the operands
             has a side-effect and must not be evaluated. *)
          | _ ->
              expr, value_unknown
          end
      | [Value_constptr x] ->
          begin match p with
            Pidentity -> const_ptr_expr expr x eid
          | Pnot -> const_bool_expr expr (x = 0) eid
          | Pisint -> const_bool_expr expr true eid
          | Poffsetint y -> const_ptr_expr expr (x + y) eid
          | Pctconst c ->
              begin
                match c with
                | Big_endian -> const_bool_expr expr Arch.big_endian eid
                | Word_size -> const_int_expr expr (8*Arch.size_int) eid
                | Int_size -> const_int_expr expr (8*Arch.size_int - 1) eid
                | Max_wosize -> const_int_expr expr ((1 lsl ((8*Arch.size_int) - 10)) - 1 ) eid
                | Ostype_unix -> const_bool_expr expr (Sys.os_type = "Unix") eid
                | Ostype_win32 -> const_bool_expr expr (Sys.os_type = "Win32") eid
                | Ostype_cygwin -> const_bool_expr expr (Sys.os_type = "Cygwin") eid
              end
          | _ ->
              expr, value_unknown
          end
      | [Value_float x] when fpc ->
          begin match p with
          | Pintoffloat -> const_int_expr expr (int_of_float x) eid
          | Pnegfloat -> const_float_expr expr (-. x) eid
          | Pabsfloat -> const_float_expr expr (abs_float x) eid
          | _ -> expr, value_unknown
          end
      | [Value_float n1; Value_float n2] when fpc ->
          begin match p with
          | Paddfloat -> const_float_expr expr (n1 +. n2) eid
          | Psubfloat -> const_float_expr expr (n1 -. n2) eid
          | Pmulfloat -> const_float_expr expr (n1 *. n2) eid
          | Pdivfloat -> const_float_expr expr (n1 /. n2) eid
          | Pfloatcomp c  -> const_comparison_expr expr c n1 n2 eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Nativeint,n)] ->
          begin match p with
          | Pintofbint Pnativeint ->
              const_int_expr expr (Nativeint.to_int n) eid
          | Pcvtbint(Pnativeint, Pint32) ->
              const_boxed_int_expr expr Int32 (Nativeint.to_int32 n) eid
          | Pcvtbint(Pnativeint, Pint64) ->
              const_boxed_int_expr expr Int64 (Int64.of_nativeint n) eid
          | Pnegbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.neg n) eid
          | Pbbswap Pnativeint ->
              const_boxed_int_expr expr Nativeint (swapnative n) eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int32,n)] ->
          begin match p with
          | Pintofbint Pint32 ->
              const_int_expr expr (Int32.to_int n) eid
          | Pcvtbint(Pint32, Pnativeint) ->
              const_boxed_int_expr expr Nativeint (Nativeint.of_int32 n) eid
          | Pcvtbint(Pint32, Pint64) ->
              const_boxed_int_expr expr Int64 (Int64.of_int32 n) eid
          | Pnegbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.neg n) eid
          | Pbbswap Pint32 ->
              const_boxed_int_expr expr Int32 (swap32 n) eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int64,n)] ->
          begin match p with
          | Pintofbint Pint64 ->
              const_int_expr expr (Int64.to_int n) eid
          | Pcvtbint(Pint64, Pnativeint) ->
              const_boxed_int_expr expr Nativeint (Int64.to_nativeint n) eid
          | Pcvtbint(Pint64, Pint32) ->
              const_boxed_int_expr expr Int32 (Int64.to_int32 n) eid
          | Pnegbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.neg n) eid
          | Pbbswap Pint64 ->
              const_boxed_int_expr expr Int64 (swap64 n) eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Nativeint,n1);
         Value_boxed_int(Nativeint,n2)] ->
          begin match p with
          | Paddbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.add n1 n2) eid
          | Psubbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.sub n1 n2) eid
          | Pmulbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.mul n1 n2) eid
          | Pdivbint Pnativeint when n2 <> 0n ->
              const_boxed_int_expr expr Nativeint (Nativeint.div n1 n2) eid
          | Pmodbint Pnativeint when n2 <> 0n ->
              const_boxed_int_expr expr Nativeint (Nativeint.rem n1 n2) eid
          | Pandbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.logand n1 n2) eid
          | Porbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.logor n1 n2) eid
          | Pxorbint Pnativeint ->
              const_boxed_int_expr expr Nativeint (Nativeint.logxor n1 n2) eid
          | Pbintcomp(Pnativeint, c) ->
              const_comparison_expr expr c n1 n2 eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int32,n1);
         Value_boxed_int(Int32,n2)] ->
          begin match p with
          | Paddbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.add n1 n2) eid
          | Psubbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.sub n1 n2) eid
          | Pmulbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.mul n1 n2) eid
          | Pdivbint Pint32 when n2 <> 0l ->
              const_boxed_int_expr expr Int32 (Int32.div n1 n2) eid
          | Pmodbint Pint32 when n2 <> 0l ->
              const_boxed_int_expr expr Int32 (Int32.rem n1 n2) eid
          | Pandbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.logand n1 n2) eid
          | Porbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.logor n1 n2) eid
          | Pxorbint Pint32 ->
              const_boxed_int_expr expr Int32 (Int32.logxor n1 n2) eid
          | Pbintcomp(Pint32, c) ->
              const_comparison_expr expr c n1 n2 eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int64,n1);
         Value_boxed_int(Int64,n2)] ->
          begin match p with
          | Paddbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.add n1 n2) eid
          | Psubbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.sub n1 n2) eid
          | Pmulbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.mul n1 n2) eid
          | Pdivbint Pint64 when n2 <> 0L ->
              const_boxed_int_expr expr Int64 (Int64.div n1 n2) eid
          | Pmodbint Pint64 when n2 <> 0L ->
              const_boxed_int_expr expr Int64 (Int64.rem n1 n2) eid
          | Pandbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.logand n1 n2) eid
          | Porbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.logor n1 n2) eid
          | Pxorbint Pint64 ->
              const_boxed_int_expr expr Int64 (Int64.logxor n1 n2) eid
          | Pbintcomp(Pint64, c) ->
              const_comparison_expr expr c n1 n2 eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Nativeint,n1);
         Value_int n2] ->
          begin match p with
          | Plslbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Nativeint
                (Nativeint.shift_left n1 n2) eid
          | Plsrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Nativeint
                (Nativeint.shift_right_logical n1 n2) eid
          | Pasrbint Pnativeint when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Nativeint
                (Nativeint.shift_right n1 n2) eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int32,n1);
         Value_int n2] ->
          begin match p with
          | Plslbint Pint32 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int32
                (Int32.shift_left n1 n2) eid
          | Plsrbint Pint32 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int32
                (Int32.shift_right_logical n1 n2) eid
          | Pasrbint Pint32 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int32
                (Int32.shift_right n1 n2) eid
          | _ -> expr, value_unknown
          end
      | [Value_boxed_int(Int64,n1);
         Value_int n2] ->
          begin match p with
          | Plslbint Pint64 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int64
                (Int64.shift_left n1 n2) eid
          | Plsrbint Pint64 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int64
                (Int64.shift_right_logical n1 n2) eid
          | Pasrbint Pint64 when 0 <= n2 && n2 < 8 * Arch.size_int ->
              const_boxed_int_expr expr Int64
                (Int64.shift_right n1 n2) eid
          | _ -> expr, value_unknown
          end
      | [Value_block _] ->
          begin match p with
          | Pisint -> const_bool_expr expr false eid
          | _ ->
              expr, value_unknown
          end
      | _ ->
          expr, value_unknown
