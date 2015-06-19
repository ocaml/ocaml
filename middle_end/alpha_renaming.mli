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

(** Alpha renaming of various identifiers. *)

(** A table used for freshening variables and static exception identifiers. *)
type t
type subst = t

(** The empty, inactive alpha renaming. *)
val empty : t

(** Activate the alpha renaming.  Without activation, operations to request
    freshenings have no effect (cf. the documentation below for
    [add_variable]).  As such, the inactive renaming is unique. *)
val activate : t -> t

(** Given the inactive alpha renaming, return the same; otherwise, return an
    empty active alpha renaming. *)
val empty_preserving_activation_state : t -> t

(** [add_variable subst var]
    If [subst] is an active substitution:
      It returns a fresh variable [new_var] and
      adds [var] -> [new_var] to the substitution.
      If a substitution [other_var] -> [var] or [symbol] -> [var] is present
      in [subst], it will also add [other_var] -> [new_var] and
      [symbol] -> [new_var].
    If [subst] is inactive, this is the identity.
*)
val add_variable : t -> Variable.t -> Variable.t * t

(** Like [add_variable], but for multiple variables, each freshened
    separately. *)
val add_variables'
   : t
  -> Variable.t list
  -> Variable.t list * t

(** Like [add_variables'], but passes through the second component of the
    input list unchanged. *)
val add_variables
   : t
  -> (Variable.t * 'a) list
  -> (Variable.t * 'a) list * t

(** As for [add_variable], but for static exception identifiers. *)
val add_static_exception : t -> Static_exception.t -> Static_exception.t * t

(** [apply_variable subst var] applies the substitution [subst] to [var].
    If no substitution is registered for [var] it is returned unchanged. *)
val apply_variable : t -> Variable.t -> Variable.t

(** As for [apply_variable], but for static exception identifiers. *)
val apply_static_exception : t -> Static_exception.t -> Static_exception.t

(** Replace recursive accesses to the closures in the set through
    [Fsymbol] by the corresponding [Fvar]. This is used to recover
    the recursive call when importing code from another compilation unit.

    If the substitution is inactive, this is the identity.
*)
val rewrite_recursive_calls_with_symbols
   : t
  -> Expr_id.t Flambda.function_declarations
  -> make_closure_symbol:(Closure_id.t -> Symbol.t)
  -> Expr_id.t Flambda.function_declarations

module Ids_and_bound_vars_of_closures : sig
  (** A table used for freshening of identifiers in [Fselect_closure]
      ("ids of closures") and Fvar_within_closure ("bound vars of closures")
      constructions.

      This information is propagated bottom up and populated when inlining a
      function containing a closure declaration.

      For instance,
        [let f x =
           let g y = ... x ... in
           ... g.x ...           (Fvar_within_closure x)
           ... g 1 ...           (Fapply (Fselect_closure g ...))
           ]

      If f is inlined, g is renamed. The approximation of g will carry this
      table such that later the access to the field x of g and selection of
      g in the closure can be substituted.
   *)
  type t

  val empty : t

  (** Freshen a closure ID based on the given renaming.  The same ID is
     returned if the renaming does not affect it. *)
  val apply_closure_id : t -> Closure_id.t -> Closure_id.t

  (** Like [apply_closure_id], but for variables within closures. *)
  val apply_var_within_closure
     : t
    -> Var_within_closure.t
    -> Var_within_closure.t
end

val apply_function_decls_and_free_vars
   : t
  -> 'a Variable.Map.t
  -> 'b Flambda.function_declarations
  -> 'a Variable.Map.t * 'b Flambda.function_declarations * t
    * Ids_and_bound_vars_of_closures.t

val toplevel_substitution
   : Variable.t Variable.Map.t
  -> 'a Flambda.t
  -> 'a Flambda.t
