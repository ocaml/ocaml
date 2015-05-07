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

open Flambda
open Abstract_identifiers

(** Lift [let] bindings to attempt to increase the length of scopes, as an
    aid to further optimizations.  For example:
      let c = let b = <expr> in b, b in fst c
    would be transformed to:
      let b = <expr> in let c = b, b in fst c
    which is then clearly just:
      <expr>
*)
val lift_lets : Expr_id.t flambda -> Expr_id.t flambda

(** Eliminate variables bound by a given closure that are not required. *)
val remove_unused_closure_variables : Expr_id.t flambda -> Expr_id.t flambda

(** Simplifies an application of a primitive based on approximation
    information. *)
val primitive
   : Lambda.primitive
  -> (Expr_id.t flambda list * (Flambdaapprox.t list))
  -> Expr_id.t flambda
  -> Debuginfo.t
  -> Expr_id.t flambda * Flambdaapprox.t

(** Simplify a sequential logical "arg1 AND arg2" expression. *)
val sequential_and
   : arg1:'a flambda
  -> arg1_approx:Flambdaapprox.t
  -> arg2:'a flambda
  -> arg2_approx:Flambdaapprox.t
  -> dbg:Debuginfo.t
  -> annot:'a
  -> 'a flambda * Flambdaapprox.t * Flambdacost.benefit

(** Like [sequential_and], but for "arg1 OR arg2". *)
val sequential_or
   : arg1:'a flambda
  -> arg1_approx:Flambdaapprox.t
  -> arg2:'a flambda
  -> arg2_approx:Flambdaapprox.t
  -> dbg:Debuginfo.t
  -> annot:'a
  -> 'a flambda * Flambdaapprox.t * Flambdacost.benefit

(** Introduce a stub function to avoid depending on unused arguments.

    For instance, it turns
      [let rec fact n unused =
         if n = 0 then 1
         else n * fact (n-1) unused]
    into
      [let rec fact' n =
         if n = 0 then 1
         else n * fact (n-1) unused
       and fact n unused = fact' n]
*)
val separate_unused_arguments_in_closures : Expr_id.t flambda -> Expr_id.t flambda

val lift_set_of_closures : Expr_id.t flambda -> Expr_id.t flambda

(** Replace setglobalfield(false, n) by ignore if the global is unused *)
val remove_unused_globals : Expr_id.t flambda -> Expr_id.t flambda
