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

(** Simple side effect analysis. *)

(* CR-someday pchambart: Replace by call to [Purity] module.
   mshinwell: Where is the [Purity] module? *)
(** Conservative approximation as to whether a given Flambda expression may
    have any side effects. *)
val no_effects : Flambda.t -> bool

val no_effects_named : Flambda.named -> bool
