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

(* Simple side effect analysis. *)

(* CR-someday pchambart: Replace by call to [Purity] module.
   mshinwell: Where is the [Purity] module? *)
(* Conservative approximation as to whether a given Flambda expression has
   any side effects. *)
val no_effects : _ Flambda.t -> bool

(* [sequence e1 e2 annot] produces a sequence expression running [e1] then
   [e2], unless [e1] has no effects, in which case the function returns
   just [e2]. *)
val sequence
   : 'a Flambda.t
  -> 'a Flambda.t
  -> 'a
  -> 'a Flambda.t
