(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  val simplify_unop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> t
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t

  val simplify_binop_int
     : Lambda.primitive
    -> t Simple_value_approx.boxed_int
    -> Flambda.named
    -> t
    -> int
    -> size_int:int
    -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t
end
