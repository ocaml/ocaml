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

val const_int_expr
   : 'a Flambda.t
  -> int
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_char_expr
   : 'a Flambda.t
  -> char
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_bool_expr
   : 'a Flambda.t
  -> bool
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_ptr_expr
   : 'a Flambda.t
  -> int
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_float_expr
   : 'a Flambda.t
  -> float
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_boxed_int_expr
   : 'a Flambda.t
  -> 'b Simple_value_approx.boxed_int
  -> 'b
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_comparison_expr
   : 'a Flambda.t
  -> Lambda.comparison
  -> 'b
  -> 'b
  -> 'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

val swap16 : int -> int
val swap32 : int32 -> int32
val swap64 : int64 -> int64
val swapnative : nativeint -> nativeint
