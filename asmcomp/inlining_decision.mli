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
open Inlining_decision_intf

(* Examine a full application of a known closure to determine whether to
   inline.  Then, if inlining is desired, perform inlining using the
   supplied helper functions [inline_by_copying_function_body] and
   [inline_by_copying_function_declaration]. *)
(* CR mshinwell: improve some of the label names to avoid confusion, e.g.
   [clos] vs. [closure]; [func] vs. [funct]. *)
val inlining_decision_for_call_site
   : env:Inlining_env.t
  -> r:Inlining_result.t
  -> clos:'a Flambda.function_declarations
  -> funct:Expr_id.t Flambda.t
  -> fun_id:Closure_id.t
  -> func:'a Flambda.function_declaration
  -> closure:Simple_value_approx.value_set_of_closures
  -> args_with_approxs:
      ((Expr_id.t Flambda.t list) * (Simple_value_approx.t list))
  -> dbg:Debuginfo.t
  -> eid:Expr_id.t
  -> inline_by_copying_function_body:'a by_copying_function_body
  -> inline_by_copying_function_declaration:'a by_copying_function_declaration
  -> loop:loop
  -> Expr_id.t Flambda.t * Inlining_result.t

(* When a function declaration is encountered in [Flambdainline], the body
   may be subject to inlining immediately, thus changing the declaration.
   This function must return [true] for that to be able to happen. *)
val should_inline_inside_declaration : _ Flambda.function_declaration -> bool
