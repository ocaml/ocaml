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

external swap16 : int -> int = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external swapnative : nativeint -> nativeint = "%bswap_native"

(* CR mshinwell: rename [eid] and/or [annot] to be consistent *)
let const_int_expr expr n eid =
  if Effect_analysis.no_effects expr then
    let (new_expr, approx) = A.make_const_int n eid in
    new_expr, approx, C.Benefit.remove_code expr C.Benefit.zero
  else expr, A.value_int n, C.Benefit.zero
let const_char_expr expr c eid =
  if Effect_analysis.no_effects expr then
    let (new_expr, approx) = A.make_const_int (Char.code c) eid in
    new_expr, approx, C.Benefit.remove_code expr C.Benefit.zero
  else expr, A.value_int (Char.code c), C.Benefit.zero
let const_ptr_expr expr n eid =
  if Effect_analysis.no_effects expr then
    let (new_expr, approx) = A.make_const_ptr n eid in
    new_expr, approx, C.Benefit.remove_code expr C.Benefit.zero
  else expr, A.value_constptr n, C.Benefit.zero
let const_bool_expr expr b eid =
  const_ptr_expr expr (if b then 1 else 0) eid
let const_float_expr expr f eid =
  if Effect_analysis.no_effects expr then
    let (new_expr, approx) = A.make_const_float f eid in
    new_expr, approx, C.Benefit.remove_code expr C.Benefit.zero
  else expr, A.value_float f, C.Benefit.zero
let const_boxed_int_expr expr t i eid =
  if Effect_analysis.no_effects expr then
    let (new_expr, approx) = A.make_const_boxed_int t i eid in
    new_expr, approx, C.Benefit.remove_code expr C.Benefit.zero
  else expr, A.value_boxed_int t i, C.Benefit.zero

let const_comparison_expr expr (cmp : Lambda.comparison) x y eid =
  const_bool_expr expr
    (match cmp with
     | Ceq -> x = y
     | Cneq -> x <> y
     | Clt -> x < y
     | Cgt -> x > y
     | Cle -> x <= y
     | Cge -> x >= y)
    eid
