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

(** Measurement of the cost (including cost in space) of Flambda terms
    in the context of inlining. *)

(* The maximum size, in some abstract measure of space cost, that an
   Flambda expression may be in order to be inlined. *)
type inlining_threshold =
  | Never_inline
  | Can_inline_if_no_larger_than of int

(* Determine whether the given Flambda expression has a sufficiently low space
   cost so as to fit under the given [inlining_threshold].  The [bonus] is
   added to the threshold before evaluation. *)
val can_inline
    : Flambda.t
  -> inlining_threshold
  -> bonus:int
  -> bool

(* CR mshinwell for pchambart: I think the name of this function might be
   misleading.  It should probably reflect the functionality it provides,
   not the use to which it is put in another module. *)
(* As for [can_inline], but returns the decision as an inlining threshold.
   If [Never_inline] is returned, the expression was too large for the
   input [inlining_threshold].  Otherwise, [Can_inline_if_no_larger_than] is
   returned, with the constructor argument being the measured estimated size
   of the expression. *)
val can_try_inlining
    : Flambda.t
  -> inlining_threshold
  -> bonus:int
  -> inlining_threshold

module Benefit : sig
  (* A model of the benefit we gain by removing a particular combination
     of operations.  Such removals are typically performed by inlining (for
     example, [remove_call]) and simplification (for example, [remove_alloc])
     passes. *)

  type t

  val zero : t
  val (+) : t -> t -> t

  val remove_call : t -> t
  val remove_alloc : t -> t
  val remove_prim : t -> t
  val remove_branch : t -> t

  val remove_code : Flambda.t -> t -> t
  val remove_code_named : Flambda.named -> t -> t

  val print : Format.formatter -> t -> unit
end

module Whether_sufficient_benefit : sig
  (* Evaluation of the benefit of removing certain operations against an
     inlining threshold. *)

  type t

  val create
     : original:Flambda.t
    -> Flambda.t
    -> Benefit.t
    -> probably_a_functor:bool
    -> t

  val evaluate : t -> bool

  val to_string : t -> string
end
