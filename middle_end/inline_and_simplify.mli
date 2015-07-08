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

(** The primary purpose of this module is to perform inlining of both
    non-recursive and recursive functions.  The inlining is directed by
    decisions made in the [Inlining_decision] module.  Readers
    interested in the strategy, rather than the technicalities, are
    advised to start reading at that module instead of this one.

    Along the way, some other optimizations and analyses are performed:
    - direct calls are identified
    - explicit closures are built for partial direct applications
    - unused static catch handlers are eliminated
    - some constants are propagated
    - some dead code is eliminated.
*)
val run
   : never_inline:bool
  -> backend:(module Backend_intf.S)
  -> Flambda.t
  -> Flambda.t
