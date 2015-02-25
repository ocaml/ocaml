(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Simple effectful test, should be replaced by call to Purity module *)
val no_effects : _ Flambda.flambda -> bool

(* [sequence e1 e2 annot] produces a sequence expression running [e1] then
   [e2], unless [e1] has no effects, in which case the function returns
   just [e2]. *)
val sequence
   : 'a Flambda.flambda
  -> 'a Flambda.flambda
  -> 'a
  -> 'a Flambda.flambda
