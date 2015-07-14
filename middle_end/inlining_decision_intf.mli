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
     env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> clos:Flambda.function_declarations
  -> lfunc:Flambda.t
  -> fun_id:Closure_id.t
  -> func:Flambda.function_declaration
  -> args:Flambda.t list
  -> Flambda.t * Inline_and_simplify_aux.Result.t

type 'a by_copying_function_declaration =
     env:Inline_and_simplify_aux.Env.t
  -> r:Inline_and_simplify_aux.Result.t
  -> funct:Flambda.t
  -> clos:Flambda.function_declarations
  -> fun_id:Closure_id.t
  -> func:Flambda.function_declaration
  -> args_with_approxs:
      (Flambda.t list) * (Simple_value_approx.t list)
  -> unchanging_params:Variable.Set.t
  -> specialised_args:Variable.Set.t
  -> dbg:Debuginfo.t
  -> (Flambda.t * Inline_and_simplify_aux.Result.t) option

type simplify =
     Inline_and_simplify_aux.Env.t
  -> Inline_and_simplify_aux.Result.t
  -> Flambda.t
  -> Flambda.t * Inline_and_simplify_aux.Result.t
