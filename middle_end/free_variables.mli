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

(** Pass in turn all free and bound variables in the given Flambda term to
    the supplied [free_variable] and [bound_variable] functions. *)
val iter
   : ?ignore_uses_as_callee:unit
  -> Flambda.t
  -> free_variable:(Variable.t -> unit)
  -> bound_variable:(Variable.t -> unit)
  -> unit

(** Calculation of the set of free variables in a given Flambda term.
    If [ignore_uses_as_callee] is specified, then uses of the variables in the
    callee position of [Apply] nodes are not counted as free occurrences.  
*)
val calculate : ?ignore_uses_as_callee:unit -> Flambda.t -> Variable.Set.t

val calculate_named : Flambda.named -> Variable.Set.t
