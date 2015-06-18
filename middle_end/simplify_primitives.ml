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

module A = Simple_value_approx
module C = Inlining_cost
module I = Simplify_boxed_integer_ops
module S = Simplify_common

let primitive (p : Lambda.primitive) (args, approxs) expr dbg ~size_int
      ~big_endian : _ Flambda.t * A.t * Inlining_cost.Benefit.t =
  let fpc = !Clflags.float_const_prop in
  match p with
  | Pmakeblock(tag, Asttypes.Immutable) ->
    let tag = Simple_value_approx.Tag.create_exn tag in
    expr, A.value_block(tag, Array.of_list approxs), C.Benefit.zero
  | Pignore -> begin
      let eid = Flambdautils.data_at_toplevel_node expr in
      match args, A.descrs approxs with
      | [arg], [(Value_int 0 | Value_constptr 0)] ->
          S.const_ptr_expr arg 0 eid
      | _ ->
          S.const_ptr_expr expr 0 eid
    end
  | _ ->
    let eid = Flambdautils.data_at_toplevel_node expr in
    match A.descrs approxs with
    | [Value_int x] ->
      begin match p with
      | Pidentity -> S.const_int_expr expr x eid
      | Pnot -> S.const_bool_expr expr (x = 0) eid
      | Pnegint -> S.const_int_expr expr (-x) eid
      | Pbswap16 -> S.const_int_expr expr (S.swap16 x) eid
      | Poffsetint y -> S.const_int_expr expr (x + y) eid
      | Pfloatofint when fpc -> S.const_float_expr expr (float_of_int x) eid
      | Pbintofint Pnativeint ->
        S.const_boxed_int_expr expr Nativeint (Nativeint.of_int x) eid
      | Pbintofint Pint32 ->
        S.const_boxed_int_expr expr Int32 (Int32.of_int x) eid
      | Pbintofint Pint64 ->
        S.const_boxed_int_expr expr Int64 (Int64.of_int x) eid
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [(Value_int x | Value_constptr x); (Value_int y | Value_constptr y)] ->
      let shift_precond = 0 <= y && y < 8 * size_int in
      begin match p with
      | Paddint -> S.const_int_expr expr (x + y) eid
      | Psubint -> S.const_int_expr expr (x - y) eid
      | Pmulint -> S.const_int_expr expr (x * y) eid
      | Pdivint when y <> 0 -> S.const_int_expr expr (x / y) eid
      | Pmodint when y <> 0 -> S.const_int_expr expr (x mod y) eid
      | Pandint -> S.const_int_expr expr (x land y) eid
      | Porint -> S.const_int_expr expr (x lor y) eid
      | Pxorint -> S.const_int_expr expr (x lxor y) eid
      | Plslint when shift_precond -> S.const_int_expr expr (x lsl y) eid
      | Plsrint when shift_precond -> S.const_int_expr expr (x lsr y) eid
      | Pasrint when shift_precond -> S.const_int_expr expr (x asr y) eid
      | Pintcomp cmp -> S.const_comparison_expr expr cmp x y eid
      | Pisout -> S.const_bool_expr expr (y > x || y < 0) eid
      (* [Psequand] and [Psequor] have special simplification rules, above. *)
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_constptr x] ->
      begin match p with
      | Pidentity -> S.const_ptr_expr expr x eid
      | Pnot -> S.const_bool_expr expr (x = 0) eid
      | Pisint -> S.const_bool_expr expr true eid
      | Poffsetint y -> S.const_ptr_expr expr (x + y) eid
      | Pctconst c ->
        begin match c with
        | Big_endian -> S.const_bool_expr expr big_endian eid
        | Word_size -> S.const_int_expr expr (8*size_int) eid
        | Int_size -> S.const_int_expr expr (8*size_int - 1) eid
        | Max_wosize ->
          (* CR mshinwell: this function should maybe not live here. *)
          S.const_int_expr expr ((1 lsl ((8*size_int) - 10)) - 1) eid
        | Ostype_unix -> S.const_bool_expr expr (Sys.os_type = "Unix") eid
        | Ostype_win32 -> S.const_bool_expr expr (Sys.os_type = "Win32") eid
        | Ostype_cygwin -> S.const_bool_expr expr (Sys.os_type = "Cygwin") eid
        end
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_float x] when fpc ->
      begin match p with
      | Pintoffloat -> S.const_int_expr expr (int_of_float x) eid
      | Pnegfloat -> S.const_float_expr expr (-. x) eid
      | Pabsfloat -> S.const_float_expr expr (abs_float x) eid
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_float n1; Value_float n2] when fpc ->
      begin match p with
      | Paddfloat -> S.const_float_expr expr (n1 +. n2) eid
      | Psubfloat -> S.const_float_expr expr (n1 -. n2) eid
      | Pmulfloat -> S.const_float_expr expr (n1 *. n2) eid
      | Pdivfloat -> S.const_float_expr expr (n1 /. n2) eid
      | Pfloatcomp c  -> S.const_comparison_expr expr c n1 n2 eid
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [A.Value_boxed_int(A.Nativeint, n)] ->
      I.Simplify_boxed_nativeint.simplify_unop p Nativeint expr n eid
    | [A.Value_boxed_int(A.Int32, n)] ->
      I.Simplify_boxed_int32.simplify_unop p Int32 expr n eid
    | [A.Value_boxed_int(A.Int64, n)] ->
      I.Simplify_boxed_int64.simplify_unop p Int64 expr n eid
    | [A.Value_boxed_int(A.Nativeint, n1);
       A.Value_boxed_int(A.Nativeint, n2)] ->
      I.Simplify_boxed_nativeint.simplify_binop p Nativeint expr n1 n2 eid
    | [A.Value_boxed_int(A.Int32, n1); A.Value_boxed_int(A.Int32, n2)] ->
      I.Simplify_boxed_int32.simplify_binop p Int32 expr n1 n2 eid
    | [A.Value_boxed_int(A.Int64, n1); A.Value_boxed_int(A.Int64, n2)] ->
      I.Simplify_boxed_int64.simplify_binop p Int64 expr n1 n2 eid
    | [A.Value_boxed_int(A.Nativeint, n1); Value_int n2] ->
      I.Simplify_boxed_nativeint.simplify_binop_int p Nativeint expr n1 n2 eid
        ~size_int
    | [A.Value_boxed_int(A.Int32, n1); Value_int n2] ->
      I.Simplify_boxed_int32.simplify_binop_int p Int32 expr n1 n2 eid
        ~size_int
    | [A.Value_boxed_int(A.Int64, n1); Value_int n2] ->
      I.Simplify_boxed_int64.simplify_binop_int p Int64 expr n1 n2 eid
        ~size_int
    | [Value_block _] when p = Pisint -> S.const_bool_expr expr false eid
    | [Value_string { size }] when p = Pstringlength ->
        S.const_int_expr expr size eid
    | [Value_string { size; contents = Some s };
       (Value_int x | Value_constptr x)] when x >= 0 && x < size ->
        begin match p with
        | Pstringrefu
        | Pstringrefs ->
            S.const_char_expr expr s.[x] eid
        | _ -> expr, A.value_unknown, C.Benefit.zero
        end
    | [Value_string { size; contents = None };
       (Value_int x | Value_constptr x)]
      when x >= 0 && x < size && p = Pstringrefs ->
        Flambda.Fprim(Pstringrefu, args, dbg, eid),
        A.value_unknown,
        (* we improved it, but there is no way to account for that: *)
        C.Benefit.zero
    | _ -> expr, A.value_unknown, C.Benefit.zero
