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

(** Simplification of sequential ("short-circuiting") operators using
    knowledge from value approximations and side effect analysis. *)

(** Simplify a sequential logical "arg1 AND arg2" expression. *)
val sequential_and
   : arg1:'a Flambda.t
  -> arg1_approx:Simple_value_approx.t
  -> arg2:'a Flambda.t
  -> arg2_approx:Simple_value_approx.t
  -> dbg:Debuginfo.t
  -> annot:'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t

(** Like [sequential_and], but for "arg1 OR arg2". *)
val sequential_or
   : arg1:'a Flambda.t
  -> arg1_approx:Simple_value_approx.t
  -> arg2:'a Flambda.t
  -> arg2_approx:Simple_value_approx.t
  -> dbg:Debuginfo.t
  -> annot:'a
  -> 'a Flambda.t * Simple_value_approx.t * Inlining_cost.Benefit.t
