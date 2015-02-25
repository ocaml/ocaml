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
  | Never_inline
  | Can_inline of int

val can_inline : _ Flambdatypes.flambda -> inline_threshold -> bonus:int -> bool
val can_try_inlining : _ Flambdatypes.flambda -> inline_threshold -> bonus:int -> inline_threshold

type benefit

val no_benefit : benefit
(* CR mshinwell: change [benefit_union] occurrences to [+] *)
val benefit_union : benefit -> benefit -> benefit
(*val (+) : benefit -> benefit -> benefit*)

val remove_call : benefit -> benefit
val remove_alloc : benefit -> benefit
val remove_prim : benefit -> benefit
val remove_branch : benefit -> benefit

val remove_code : _ Flambdatypes.flambda -> benefit -> benefit

val sufficient_benefit_for_inline : _ Flambdatypes.flambda -> benefit -> inline_threshold -> bool

val print_benefit : Format.formatter -> benefit -> unit
