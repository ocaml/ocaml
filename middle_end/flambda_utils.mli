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

val name_expr : ?name:string -> Flambda.named -> Flambda.t

val compare_const : Flambda.const -> Flambda.const -> int

val initialize_symbols
   : Flambda.program
  -> (Symbol.t * Tag.t * Flambda.t list) list

val imported_symbols : Flambda.program -> Symbol.Set.t

val needed_import_symbols : Flambda.program -> Symbol.Set.t

val introduce_needed_import_symbols : Flambda.program -> Flambda.program

val root_symbol : Flambda.program -> Symbol.t

(** Returns [true] iff the given term might raise the given static
    exception. *)
val might_raise_static_exn : Flambda.named -> Static_exception.t -> bool

(** Creates a map from closure IDs to function declarations by iterating over
    all sets of closures in the given program. *)
val make_closure_map
   : Flambda.program
  -> Flambda.function_declarations Closure_id.Map.t

(** Like [make_closure_map], but takes a mapping from set of closures IDs to
    function declarations, instead of a [program]. *)
val make_closure_map'
   : Flambda.function_declarations Set_of_closures_id.Map.t
  -> Flambda.function_declarations Closure_id.Map.t

(** The definitions of all constants that have been lifted out to [Let_symbol]
    or [Let_rec_symbol] constructions. *)
val all_lifted_constants
   : Flambda.program
  -> (Symbol.t * Flambda.constant_defining_value) list

(** Like [all_lifted_constant_symbols], but returns a map instead of a list. *)
val all_lifted_constants_as_map
   : Flambda.program
  -> Flambda.constant_defining_value Symbol.Map.t

(** The identifiers of all constant sets of closures that have been lifted out
    to [Let_symbol] or [Let_rec_symbol] constructions. *)
val all_lifted_constant_sets_of_closures
   : Flambda.program
  -> Set_of_closures_id.Set.t

(** All sets of closures in the given program (whether or not bound to a
    symbol.) *)
val all_sets_of_closures : Flambda.program -> Flambda.set_of_closures list

val make_variable_symbol : Variable.t -> Symbol.t

(* CR pchambart: this function should probably not be named like that:
   it is accessing the first field of that symbol. Also a more general
   version of that function may take a [named] instead of a symbol and
   be called with [Read_symbol_field (symbol, 0)]. *)
val substitute_variable_to_symbol
   : Symbol.t Variable.Map.t
  -> Flambda.t
  -> Flambda.t

(** For the compilation of switch statements. *)
module Switch_storer : sig
  val mk_store : unit -> Flambda.t Switch.t_store
end

(** Within a set of function declarations there is a set of function bodies,
    each of which may (or may not) reference one of the other functions in
    the same set.  Initially such intra-set references are by [Var]s (known
    as "fun_var"s) but if the function is lifted by [Lift_constants] then the
    references will be translated to [Symbol]s.  This means that optimization
    passes that need to identify whether a given "fun_var" (i.e. a key in the
    [funs] map in a value of type [function_declarations]) is used in one of
    the function bodies need to examine the [free_symbols] as well as the
    [free_variables] members of [function_declarations].  This function makes
    that process easier by computing all used "fun_var"s in the bodies of
    the given set of function declarations, including the cases where the
    references are [Symbol]s.  The returned value is a map from "fun_var"s
    to the "fun_var"s (if any) used in the body of the function associated
    with that "fun_var".
    If [only_via_symbols] is set, references in the bodies to [fun_var]s that
    are [Variable]s are ignored.
*)
val fun_vars_referenced_in_decls
   : ?only_via_symbols:unit
  -> Flambda.function_declarations
  -> backend:(module Backend_intf.S)
  -> Variable.Set.t Variable.Map.t
