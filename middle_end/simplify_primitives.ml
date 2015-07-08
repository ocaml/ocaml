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
      ~big_endian : Flambda.named * A.t * Inlining_cost.Benefit.t =
  let fpc = !Clflags.float_const_prop in
  match p with
  | Pmakeblock(tag, Asttypes.Immutable) ->
    let tag = Tag.create_exn tag in
    expr, A.value_block(tag, Array.of_list approxs), C.Benefit.zero
  | Pignore -> begin
      match args, A.descrs approxs with
      | [arg], [(Value_int 0 | Value_constptr 0)] ->
        S.const_ptr_expr (Flambda.Expr (Var arg)) 0
      | _ -> S.const_ptr_expr expr 0
    end
  | _ ->
    match A.descrs approxs with
    | [Value_int x] ->
      begin match p with
      | Pidentity -> S.const_int_expr expr x
      | Pnot -> S.const_bool_expr expr (x = 0)
      | Pnegint -> S.const_int_expr expr (-x)
      | Pbswap16 -> S.const_int_expr expr (S.swap16 x)
      | Poffsetint y -> S.const_int_expr expr (x + y)
      | Pfloatofint when fpc -> S.const_float_expr expr (float_of_int x)
      | Pbintofint Pnativeint ->
        S.const_boxed_int_expr expr Nativeint (Nativeint.of_int x)
      | Pbintofint Pint32 -> S.const_boxed_int_expr expr Int32 (Int32.of_int x)
      | Pbintofint Pint64 -> S.const_boxed_int_expr expr Int64 (Int64.of_int x)
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [(Value_int x | Value_constptr x); (Value_int y | Value_constptr y)] ->
      let shift_precond = 0 <= y && y < 8 * size_int in
      begin match p with
      | Paddint -> S.const_int_expr expr (x + y)
      | Psubint -> S.const_int_expr expr (x - y)
      | Pmulint -> S.const_int_expr expr (x * y)
      | Pdivint when y <> 0 -> S.const_int_expr expr (x / y)
      | Pmodint when y <> 0 -> S.const_int_expr expr (x mod y)
      | Pandint -> S.const_int_expr expr (x land y)
      | Porint -> S.const_int_expr expr (x lor y)
      | Pxorint -> S.const_int_expr expr (x lxor y)
      | Plslint when shift_precond -> S.const_int_expr expr (x lsl y)
      | Plsrint when shift_precond -> S.const_int_expr expr (x lsr y)
      | Pasrint when shift_precond -> S.const_int_expr expr (x asr y)
      | Pintcomp cmp -> S.const_comparison_expr expr cmp x y
      | Pisout -> S.const_bool_expr expr (y > x || y < 0)
      (* [Psequand] and [Psequor] have special simplification rules, above. *)
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_constptr x] ->
      begin match p with
      | Pidentity -> S.const_ptr_expr expr x
      | Pnot -> S.const_bool_expr expr (x = 0)
      | Pisint -> S.const_bool_expr expr true
      | Poffsetint y -> S.const_ptr_expr expr (x + y)
      | Pctconst c ->
        begin match c with
        | Big_endian -> S.const_bool_expr expr big_endian
        | Word_size -> S.const_int_expr expr (8*size_int)
        | Int_size -> S.const_int_expr expr (8*size_int - 1)
        | Max_wosize ->
          (* CR mshinwell: this function should maybe not live here. *)
          S.const_int_expr expr ((1 lsl ((8*size_int) - 10)) - 1)
        | Ostype_unix -> S.const_bool_expr expr (Sys.os_type = "Unix")
        | Ostype_win32 -> S.const_bool_expr expr (Sys.os_type = "Win32")
        | Ostype_cygwin -> S.const_bool_expr expr (Sys.os_type = "Cygwin")
        end
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_float x] when fpc ->
      begin match p with
      | Pintoffloat -> S.const_int_expr expr (int_of_float x)
      | Pnegfloat -> S.const_float_expr expr (-. x)
      | Pabsfloat -> S.const_float_expr expr (abs_float x)
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [Value_float n1; Value_float n2] when fpc ->
      begin match p with
      | Paddfloat -> S.const_float_expr expr (n1 +. n2)
      | Psubfloat -> S.const_float_expr expr (n1 -. n2)
      | Pmulfloat -> S.const_float_expr expr (n1 *. n2)
      | Pdivfloat -> S.const_float_expr expr (n1 /. n2)
      | Pfloatcomp c  -> S.const_comparison_expr expr c n1 n2
      | _ -> expr, A.value_unknown, C.Benefit.zero
      end
    | [A.Value_boxed_int(A.Nativeint, n)] ->
      I.Simplify_boxed_nativeint.simplify_unop p Nativeint expr n
    | [A.Value_boxed_int(A.Int32, n)] ->
      I.Simplify_boxed_int32.simplify_unop p Int32 expr n
    | [A.Value_boxed_int(A.Int64, n)] ->
      I.Simplify_boxed_int64.simplify_unop p Int64 expr n
    | [A.Value_boxed_int(A.Nativeint, n1);
       A.Value_boxed_int(A.Nativeint, n2)] ->
      I.Simplify_boxed_nativeint.simplify_binop p Nativeint expr n1 n2
    | [A.Value_boxed_int(A.Int32, n1); A.Value_boxed_int(A.Int32, n2)] ->
      I.Simplify_boxed_int32.simplify_binop p Int32 expr n1 n2
    | [A.Value_boxed_int(A.Int64, n1); A.Value_boxed_int(A.Int64, n2)] ->
      I.Simplify_boxed_int64.simplify_binop p Int64 expr n1 n2
    | [A.Value_boxed_int(A.Nativeint, n1); Value_int n2] ->
      I.Simplify_boxed_nativeint.simplify_binop_int p Nativeint expr n1 n2
        ~size_int
    | [A.Value_boxed_int(A.Int32, n1); Value_int n2] ->
      I.Simplify_boxed_int32.simplify_binop_int p Int32 expr n1 n2
        ~size_int
    | [A.Value_boxed_int(A.Int64, n1); Value_int n2] ->
      I.Simplify_boxed_int64.simplify_binop_int p Int64 expr n1 n2
        ~size_int
    | [Value_block _] when p = Lambda.Pisint ->
      S.const_bool_expr expr false
    | [Value_string { size }] when p = Lambda.Pstringlength ->
      S.const_int_expr expr size
    | [Value_string { size; contents = Some s };
       (Value_int x | Value_constptr x)] when x >= 0 && x < size ->
        begin match p with
        | Pstringrefu
        | Pstringrefs -> S.const_char_expr expr s.[x]
        | _ -> expr, A.value_unknown, C.Benefit.zero
        end
    | [Value_string { size; contents = None };
       (Value_int x | Value_constptr x)]
      when x >= 0 && x < size && p = Lambda.Pstringrefs ->
        Flambda.Prim (Pstringrefu, args, dbg),
          A.value_unknown,
          (* we improved it, but there is no way to account for that: *)
          C.Benefit.zero
    | _ -> expr, A.value_unknown, C.Benefit.zero
