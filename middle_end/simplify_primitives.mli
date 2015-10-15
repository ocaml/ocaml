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

(** Simplifies an application of a primitive based on approximation
    information. *)
val primitive
   : Lambda.primitive
  -> (Variable.t list * (Simple_value_approx.t list))
  -> Flambda.named
  -> Debuginfo.t
  -> size_int:int
  -> big_endian:bool
  -> Flambda.named * Simple_value_approx.t * Inlining_cost.Benefit.t
