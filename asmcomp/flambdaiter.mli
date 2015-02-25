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
open Flambdatypes

val apply_on_subexpressions : ('a flambda -> unit) ->
  'a flambda -> unit

val subexpressions : 'a flambda -> 'a flambda list

val iter : ('a flambda -> unit) -> 'a flambda -> unit

val iter_toplevel : ('a flambda -> unit) -> 'a flambda -> unit
(** [iter_toplevel f t] Apply f on every toplevel subexpression of t,
    i.e. does not apply it on functions body *)

val iter_on_closures :
  ('a fset_of_closures -> 'a -> unit) -> 'a flambda -> unit

val map : ('a flambda -> 'a flambda) ->
  'a flambda -> 'a flambda

val map_toplevel : ('a flambda -> 'a flambda) ->
  'a flambda -> 'a flambda

val free_variables : 'a flambda -> Variable.Set.t

val fold_subexpressions :
  ('acc -> Variable.Set.t -> 'a flambda -> 'acc * 'a flambda) -> 'acc -> 'a flambda ->
  'acc * 'a flambda

val expression_free_variables : 'a flambda -> Variable.Set.t

val subexpression_bound_variables : 'a flambda -> (Variable.Set.t*'a flambda) list

val map_data : ('a -> 'b) -> 'a flambda -> 'b flambda
