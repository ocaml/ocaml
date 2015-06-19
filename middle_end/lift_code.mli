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

type lifter = Expr_id.t Flambda.t -> Expr_id.t Flambda.t

(** Lift [let] bindings to attempt to increase the length of scopes, as an
    aid to further optimizations.  For example:
      let c = let b = <expr> in b, b in fst c
    would be transformed to:
      let b = <expr> in let c = b, b in fst c
    which is then clearly just:
      <expr>
*)
val lift_lets : lifter

(* CR mshinwell: add comment *)
val lift_set_of_closures : lifter

(* Transform a [Pmakeblock] operation, that allocates and fills a new block,
   to a sequence of [let]s.  The aim is to then eliminate the allocation of
   the block, so long as it does not escape.  For example,

     Pmakeblock [expr_0; ...; expr_n]

   is transformed to:

     let x_0 = expr_0 in
     ...
     let x_n = expr_n in
     Pmakeblock [x_0; ...; x_n]

   A more general solution would be to convert completely to ANF.
*)
val lift_block_construction_to_variables : lifter

(* Enforce right-to-left evaluation of function arguments by lifting the
   expressions computing the arguments into [let]s. *)
val lift_apply_construction_to_variables : lifter
