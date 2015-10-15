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

(** [const_*_expr expr v annot], where the expression [expr] is known to
    evaluate to the value [v], attempt to produce a more simple expression
    together with its approximation and the benefit gained by replacing [expr]
    with this new expression.  This simplification is only performed if [expr]
    is known to have no side effects.  Otherwise, [expr] itself is returned,
    with an appropriate approximation but zero benefit.

    [const_boxed_int_expr] takes an additional argument specifying the kind of
    boxed integer to which the given expression evaluates.
*)

val const_int_expr
   : Flambda.named
  -> int
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_char_expr
   : Flambda.named
  -> char
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_bool_expr
   : Flambda.named
  -> bool
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_ptr_expr
   : Flambda.named
  -> int
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_float_expr
   : Flambda.named
  -> float
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

val const_boxed_int_expr
   : Flambda.named
  -> 'a Simple_value_approx.boxed_int
  -> 'a
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

(* CR mshinwell: This function is horrible, it uses polymorphic comparisons
   (including on floats).  Apart from fixing it, we should also take care to
   document exactly which floating-point comparison semantics is being
   used. *)
val const_comparison_expr
   : Flambda.named
  -> Lambda.comparison
  -> 'a
  -> 'a
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

(** Functions for transposing the order of bytes within words of various
    sizes. *)
val swap16 : int -> int
val swap32 : int32 -> int32
val swap64 : int64 -> int64
val swapnative : nativeint -> nativeint
