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

(** Utility functions for the Flambda intermediate language. *)

(** Access functions *)

(** [find_declaration f decl] raises [Not_found] if [f] is not in [decl]. *)
val find_declaration :
  Closure_id.t -> Flambda.function_declarations -> Flambda.function_declaration

(** [find_declaration_variable f decl] raises [Not_found] if [f] is not in
    [decl]. *)
val find_declaration_variable :
  Closure_id.t -> Flambda.function_declarations -> Variable.t

(** [find_free_variable v clos] raises [Not_found] if [c] is not in [clos]. *)
val find_free_variable :
  Var_within_closure.t -> Flambda.set_of_closures -> Variable.t

(** Utility functions *)

val function_arity : Flambda.function_declaration -> int

(** Variables "bound by a closure" are those variables free in the
    corresponding function's body that are neither:
    - bound as parameters of that function; nor
    - bound by the [let] binding that introduces the function declaration(s).
    In particular, if [f], [g] and [h] are being introduced by a
    simultaneous, possibly mutually-recursive [let] binding then none of
    [f], [g] or [h] are bound in any of the closures for [f], [g] and [h].
*)
val variables_bound_by_the_closure :
  Closure_id.t -> Flambda.function_declarations -> Variable.Set.t

(** If [can_be_merged f1 f2] is [true], it is safe to merge switch
    branches containing [f1] and [f2]. *)
val can_be_merged : Flambda.t -> Flambda.t -> bool

val description_of_toplevel_node : Flambda.t -> string

(** Sharing key *)
(* CR mshinwell for pchambart: this needs a proper comment as discussed *)
type sharing_key
val make_key : Flambda.t -> sharing_key option

(* Given an expression, freshen all variables within it, and form a function
   whose body is the resulting expression.  The variables specified by
   [params] will become the parameters of the function; the closure will be
   identified by [id].  [params] must only reference variables that are
   free variables of [body]. *)
(* CR mshinwell: consider improving name and names of arguments *)
val make_closure_declaration
   : id:Variable.t
  -> body:Flambda.t
  -> params:Variable.t list
  -> Flambda.t

val toplevel_substitution
   : Variable.t Variable.Map.t
  -> Flambda.t
  -> Flambda.t

(** [bind [var1, expr1; ...; varN, exprN] body] binds using
    [Immutable] [Let] expressions the given [(var, expr)] pairs around the
    body. *)
val bind
   : bindings:(Variable.t * Flambda.named) list
  -> body:Flambda.t
  -> Flambda.t

val name_expr : Flambda.named -> Flambda.t

val compare_const : Flambda.const -> Flambda.const -> int

val constant_symbol_declarations
  : Flambda.program ->
  (Symbol.t * Flambda.constant_defining_value) list

val initialize_symbols
  : Flambda.program ->
  (Symbol.t * Tag.t * Flambda.t list) list
