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
val add_sb_sym : t -> Symbol.SymbolMap.key -> Variable.t -> t
val add_sb_var : t -> Variable.t -> Variable.t -> t
val add_sb_exn : t -> Static_exception.Map.key -> Static_exception.t -> t
val sb_exn : t -> Static_exception.Map.key -> Static_exception.Map.key
val new_subst_exn :
  t ->
  Static_exception.Map.key ->
  Static_exception.Map.key * t
val new_subst_id : t -> Variable.t -> Variable.t * t
val new_subst_ids :
  t ->
  (Variable.t * 'a) list ->
  (Variable.t * 'a) list * t
val new_subst_ids' :
  t ->
  Variable.t list ->
  Variable.t list * t

val find_var : t -> Variable.t -> Variable.t
val find_var_exn : t -> Variable.t -> Variable.t
val find_symbol_exn : t -> Symbol.t -> Variable.t

val freshen_var : Variable.t -> Variable.t
val subst_var : t -> Variable.t -> Variable.t

module Alpha_renaming_map_for_ids_and_bound_vars_of_closures : sig
  type t
  val empty : t
  val new_subst_fv :
    t ->
    Variable.t ->
    subst -> Variable.t * subst * t
  val new_subst_fun :
    t ->
    Variable.t ->
    subst -> Variable.t * subst * t
  val subst_free_vars :
    'a Variable.Map.t ->
    subst -> 'a Variable.Map.t * subst * t
  val ffuns_subst :
    t ->
    subst ->
    'a Flambda.function_declarations ->
    'a Flambda.function_declarations * subst * t

  val fun_off_id : t -> Closure_id.t -> Closure_id.t
  val fv_off_id : t -> Var_within_closure.t -> Var_within_closure.t
end
