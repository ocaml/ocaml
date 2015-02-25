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

(** Intermediate language used to perform closure conversion and inlining.
    See flambdatypes.ml for documentation.
*)

include module type of Flambdatypes

(** Access functions *)

val find_declaration :
  Closure_id.t -> 'a function_declarations -> 'a function_declaration
(** [find_declaration f decl] raises [Not_found] if [f] is not in [decl]. *)

val find_declaration_variable :
  Closure_id.t -> 'a function_declarations -> Variable.t
(** [find_declaration_variable f decl] raises [Not_found] if [f] is not in
    [decl]. *)

val find_free_variable :
  Var_within_closure.t -> 'a fset_of_closures -> 'a flambda
(** [find_free_variable v clos] raises [Not_found] if [c] is not in [clos]. *)

(** Utility functions *)

val function_arity : 'a function_declaration -> int

val variables_bound_by_the_closure :
  Closure_id.t -> 'a function_declarations -> Variable.Set.t
(** Variables "bound by a closure" are those variables free in the
    corresponding function's body that are neither:
    - bound as parameters of that function; nor
    - bound by the [let] binding that introduces the function declaration(s).
    In particular, if [f], [g] and [h] are being introduced by a
    simultaneous, possibly mutually-recursive [let] binding then none of
    [f], [g] or [h] are bound in any of the closures for [f], [g] and [h].
*)

val can_be_merged : 'a flambda -> 'a flambda -> bool
(** If [can_be_merged f1 f2] is [true], it is safe to merge switch
    branches containing [f1] and [f2]. *)

val data_at_toplevel_node : 'a flambda -> 'a

val description_of_toplevel_node : 'a flambda -> string

val recursive_functions : 'a function_declarations -> Variable.Set.t
(** Recursive functions are the one that can call themselves or call
    a function that can.

    for instance in this simultaneous definition of [f] [g] and [h],
    [f] and [g] are recursives, but not [h]
      [let rec f x = g x
       and g x = f x
       and h x = g x]
*)

(** Sharing key *)
(* CR mshinwell for pchambart: this needs a proper comment as discussed *)
type sharing_key
val make_key : 'a flambda -> sharing_key option

(* Fold over the variables bound by a given closure, at the same time
   creating [Fvariable_in_closure] expressions to access them. *)
val fold_over_exprs_for_variables_bound_by_closure
   : fun_id:Closure_id.t
  -> clos_id:Variable.t
  -> clos:'a function_declarations
  -> init:'b
  -> f:(acc:'b -> var:Variable.t -> expr:Expr_id.t flambda -> 'b)
  -> 'b
