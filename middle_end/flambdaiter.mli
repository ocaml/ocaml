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

(** Apply the given functions to the immediate subexpressions of the given
    Flambda expression.  For avoidance of doubt, if a subexpression is
    [Expr], it is passed to the function taking [Flambda.named], rather
    than being followed and passed to the function taking [Flambda.t]. *)
val apply_on_subexpressions
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val iter
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

(** [iter_toplevel f t] applies [f] on every toplevel subexpression of [t].
    In particular, it never applies [f] to the body of a function (which
    will always be contained within an [Set_of_closures] expression). *)
val iter_toplevel
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

(* CR mshinwell: rename to iter_on_set_of_closures *)
val iter_on_sets_of_closures
   : (Flambda.set_of_closures -> unit)
  -> Flambda.t
  -> unit

val map
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_toplevel
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t
