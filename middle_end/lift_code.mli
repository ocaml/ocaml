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

type lifter = Flambda.t -> Flambda.t

(** Lift [let] bindings to attempt to increase the length of scopes, as an
    aid to further optimizations.  For example:
      let c = let b = <expr> in b, b in fst c
    would be transformed to:
      let b = <expr> in let c = b, b in fst c
    which is then clearly just:
      <expr>
*)
val lift_lets : lifter

(* CR mshinwell: Rename to [bind]?  Also see Flambda_utils.bind. *)
(* [create_body] always receives the variables corresponding to [evaluate]
   in the same order.  However [evaluation_order] specifies in which order
   the (possibly complex) expressions bound to those variables are
   evaluated. *)
val lifting_helper
   : Flambda.t list
  -> evaluation_order:[ `Left_to_right | `Right_to_left ]
  -> create_body:(Variable.t list -> Flambda.t)
  -> name:string
  -> Flambda.t
