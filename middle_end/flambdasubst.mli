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

open Abstract_identifiers

type t
type subst = t

val empty : t
val new_substitution : t -> t
val activate : t -> t
val sb_exn : t -> Static_exception.Map.key -> Static_exception.Map.key
val new_subst_exn :
  t ->
  Static_exception.Map.key ->
  Static_exception.Map.key * t
val new_subst_id : t -> Variable.t -> Variable.t * t
(** [new_subst_id subst var]
    If [subst] is an active substitution:
      It retuns a fresh variable [new_var] and
      adds [var] -> [new_var] to the substitution.
      If a substitution [other_var] -> [var] or [symbol] -> [var] is present
      in [subst], it will also add [other_var] -> [new_var] and
      [symbo] -> [new_var].
    If [subst] is inactive, this is the identity.
*)
val new_subst_ids :
  t ->
  (Variable.t * 'a) list ->
  (Variable.t * 'a) list * t
val new_subst_ids' :
  t ->
  Variable.t list ->
  Variable.t list * t

(** [subst_var subst var] apply the substitution [subst] to [var].
    If no substitution is registered for [var] it is returned unchanged. *)
val subst_var : t -> Variable.t -> Variable.t

val freshen_var : Variable.t -> Variable.t

val rewrite_recursive_calls_with_symbols
   : t
  -> Expr_id.t Flambda.function_declarations
  -> make_closure_symbol:(Closure_id.t -> Symbol.t)
  -> Expr_id.t Flambda.function_declarations

(** Replace recursive access to the closures in the set through
    [Fsymbol] by the corresponding [Fvar]. This is used to recover
    the recursive call when importing code from another compilation unit.

    If the substitution is inactive, this is the identity.
 *)

module Alpha_renaming_map_for_ids_and_bound_vars_of_closures : sig
  (* Tables used for identifiers substitution in
     Fselect_closure ("ids of closures") and Fvar_within_closure ("bound vars
     of closures") constructions.
     This information is propagated bottom up. This is
     populated when inlining a function containing a closure
     declaration.

     For instance,
       [let f x =
          let g y = ... x ... in
          ... g.x ...           (Fvar_within_closure x)
          ... g 1 ...           (FApply (Fselect_closure g ...))
          ]
     if f is inlined g is renamed. The approximation of g will
     cary this table such that later the access to the field x
     of g and selection of g in the closure can be substituted.
   *)

  type t
  val empty : t

  val subst_function_declarations_and_free_variables :
    subst ->
    'a Variable.Map.t ->
    'b Flambda.function_declarations ->
    'a Variable.Map.t * 'b Flambda.function_declarations * subst * t

  val subst_closure_id : t -> Closure_id.t -> Closure_id.t
  val subst_var_within_closure :
    t ->
    Var_within_closure.t ->
    Var_within_closure.t
end

val toplevel_substitution
   : Variable.t Variable.Map.t
  -> 'a Flambda.t
  -> 'a Flambda.t
