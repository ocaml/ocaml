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

(* CR mshinwell: we need to document whether these iterators follow any
   particular order. *)

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

val iter_expr
   : (Flambda.t -> unit)
  -> Flambda.t
  -> unit

(* CR-someday mshinwell: we might need to add the corresponding variable to
   the parameters of the user function for [iter_named] *)
val iter_named
   : (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val iter_named_on_named
   : (Flambda.named -> unit)
  -> Flambda.named
  -> unit

(** [iter_toplevel f t] applies [f] on every toplevel subexpression of [t].
    In particular, it never applies [f] to the body of a function (which
    will always be contained within an [Set_of_closures] expression). *)
val iter_toplevel
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.t
  -> unit

val iter_named_toplevel
   : (Flambda.t -> unit)
  -> (Flambda.named -> unit)
  -> Flambda.named
  -> unit

(* CR mshinwell: rename to iter_on_set_of_closures *)
val iter_on_sets_of_closures
   : (Flambda.set_of_closures -> unit)
  -> Flambda.t
  -> unit

val iter_all_let_and_let_rec_bindings
   : Flambda.t
  -> f:(Variable.t -> Flambda.named -> unit)
  -> unit

val iter_exprs_at_toplevel_of_program
   : Flambda.program
  -> f:(Flambda.t -> unit)
  -> unit

val map
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_expr
   : (Flambda.t -> Flambda.t)
  -> Flambda.t
  -> Flambda.t

val map_named
   : (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_toplevel
   : (Flambda.t -> Flambda.t)
  -> (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_toplevel_named
   : (Flambda.named -> Flambda.named)
  -> Flambda.t
  -> Flambda.t

val map_symbols
   : Flambda.t
  -> f:(Symbol.t -> Symbol.t)
  -> Flambda.t

val map_toplevel_sets_of_closures
   : Flambda.t
  -> f:(Flambda.set_of_closures -> Flambda.set_of_closures)
  -> Flambda.t

val map_apply
   : Flambda.t
  -> f:(Flambda.apply -> Flambda.apply)
  -> Flambda.t

val map_function_bodies
   : Flambda.set_of_closures
  -> f:(Flambda.t -> Flambda.t)
  -> Flambda.set_of_closures

val map_sets_of_closures
   : Flambda.t
  -> f:(Flambda.set_of_closures -> Flambda.set_of_closures)
  -> Flambda.t

val map_project_var_to_expr_opt
   : Flambda.t
  -> f:(Flambda.project_var -> Flambda.t option)
  -> Flambda.t

val map_exprs_at_toplevel_of_program
   : Flambda.program
  -> f:(Flambda.t -> Flambda.t)
  -> Flambda.program
