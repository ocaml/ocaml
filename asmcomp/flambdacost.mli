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

(** Measurement of the cost (including cost in space) of flambda terms. *)

type inline_threshold =
  | No_inline
  | Can_inline of int

val can_inline : _ Flambda.flambda -> inline_threshold -> bonus:int -> bool
val can_try_inlining : _ Flambda.flambda -> inline_threshold -> bonus:int -> inline_threshold
