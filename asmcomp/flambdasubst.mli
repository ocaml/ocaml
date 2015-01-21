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
val activate : t -> t
val add_sb_sym : t -> Symbol.SymbolMap.key -> Variable.Map.key -> t
val add_sb_var : t -> Variable.Map.key -> Variable.Map.key -> t
val add_sb_exn : t -> Static_exception.Map.key -> Static_exception.t -> t
val sb_exn : t -> Static_exception.Map.key -> Static_exception.Map.key
val new_subst_exn :
  t ->
  Static_exception.Map.key ->
  Static_exception.Map.key * t
val new_subst_id : t -> Variable.Map.key -> Variable.Map.key * t
val new_subst_ids :
  t ->
  (Variable.Map.key * 'a) list ->
  (Variable.Map.key * 'a) list * t
val new_subst_ids' :
  t ->
  Variable.Map.key list ->
  Variable.Map.key list * t
val find_subst' : t -> Variable.Map.key -> Variable.t
val subst_var : t -> Variable.Map.key -> Variable.Map.key

module Alpha_renaming_map_for_ids_and_bound_vars_of_closures : sig
  type t
  val empty : t
  val new_subst_fv :
    t ->
    Variable.Map.key ->
    subst -> Variable.Map.key * subst * t
  val new_subst_fun :
    t ->
    Variable.Map.key ->
    subst -> Variable.Map.key * subst * t
  val subst_free_vars :
    'a Variable.Map.t ->
    subst -> 'a Variable.Map.t * subst * t
  val ffuns_subst :
    t ->
    subst ->
    'a Flambda.function_declarations ->
    'a Flambda.function_declarations * subst * t
end
