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

type 'a by_copying_function_body =
     env:Inlining_env.t
  -> r:Inlining_result.t
  -> clos:'a Flambda.function_declarations
  -> lfunc:Expr_id.t Flambda.t
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> args:Expr_id.t Flambda.t list
  -> Expr_id.t Flambda.t * Inlining_result.t

type 'a by_copying_function_declaration =
     env:Inlining_env.t
  -> r:Inlining_result.t
  -> funct:Expr_id.t Flambda.t
  -> clos:'a Flambda.function_declarations
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> args_with_approxs:
      (Expr_id.t Flambda.t list) * (Simple_value_approx.t list)
  -> unchanging_params:Variable.Set.t
  -> specialised_args:Variable.Set.t
  -> dbg:Debuginfo.t
  -> (Expr_id.t Flambda.t * Inlining_result.t) option

type loop =
     Inlining_env.t
  -> Inlining_result.t
  -> Expr_id.t Flambda.t
  -> Expr_id.t Flambda.t * Inlining_result.t
