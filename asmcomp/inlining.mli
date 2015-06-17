(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The primary purpose of this function is to perform inlining of both
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
val inline
   : never_inline:bool
  -> Abstract_identifiers.Expr_id.t Flambda.t
  -> Abstract_identifiers.Expr_id.t Flambda.t
