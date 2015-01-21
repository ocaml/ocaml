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

val lambda_smaller' : _ Flambda.flambda -> than:int -> int option
val lambda_smaller : _ Flambda.flambda -> than:int -> bool
