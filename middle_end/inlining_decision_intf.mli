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

(* CR mshinwell: name of this source file could now be improved *)

type 'a by_copying_function_body =
     env:Inlining_env.t
  -> r:Inlining_result.t
  -> clos:'a Flambda.function_declarations
  -> lfunc:Flambda.t
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> args:Flambda.t list
  -> Flambda.t * Inlining_result.t

type 'a by_copying_function_declaration =
     env:Inlining_env.t
  -> r:Inlining_result.t
  -> funct:Flambda.t
  -> clos:'a Flambda.function_declarations
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> args_with_approxs:
      (Flambda.t list) * (Simple_value_approx.t list)
  -> unchanging_params:Variable.Set.t
  -> specialised_args:Variable.Set.t
  -> dbg:Debuginfo.t
  -> (Flambda.t * Inlining_result.t) option

type simplify =
     Inlining_env.t
  -> Inlining_result.t
  -> Flambda.t
  -> Flambda.t * Inlining_result.t
