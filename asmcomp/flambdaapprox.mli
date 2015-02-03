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
open Flambda

(** Value approximation used during inlining. *)

type tag = int

type 'a boxed_int = 'a Flambdaexport.boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * t array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_set_of_closures of value_set_of_closures
  | Value_closure of value_offset
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t
  | Value_unresolved of Symbol.t (* No description was found for this symbol *)

and value_offset = {
  fun_id : Closure_id.t;
  set_of_closures : value_set_of_closures;
}

and value_set_of_closures = {
  ffunctions : Expr_id.t function_declarations;
  bound_var : t Var_within_closure.Map.t;
  kept_params : Variable.Set.t;
  ffunction_sb :
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures.t;
}

(* CXR mshinwell for pchambart: call this type [t]
   pchambart: done *)
and t = {
  descr : descr;
  var : Variable.t option;
  symbol : Symbol.t option;
}
(** A value of type [t] corresponds to an approximation of a value.
    Such approximations are deduced at particular points in an expression
    tree, but may subsequently be propagated to other locations.

    At the point at which an approximation is built for some value [v], we can
    construct a set of variables (call the set [S]) that are known to alias the
    same value [v].  Each member of [S] will have the same or a more precise
    [descr] field in its approximation relative to the approximation for [v].
    (An increase in precision may currently be introduced for pattern
    matches.)  If [S] is non-empty then it is guaranteed that there is a
    unique member of [S] that was declared in a scope further out ("earlier")
    than all other members of [S].  If such a member exists then it is
    recorded in the [var] field.  Otherwise [var] is [None].

    Analogous to the construction of the set [S], we can construct a set [T]
    consisting of all symbols that are known to alias the value whose
    approximation is being constructed.  If [T] is non-empty then the
    [symbol] field is set to some member of [T]; it does not matter which
    one.  (There is no notion of scope for symbols.)
*)

(** Smart constructors *)

val value_unknown : t
val value_int : int -> t
val value_float : float -> t
val value_boxed_int : 'i boxed_int -> 'i -> t
val value_constptr : int -> t
val value_closure : value_offset -> t
(* CXR mshinwell for pchambart: update name of [value_unoffseted_closure]
   pchambart: done *)
val value_set_of_closures : value_set_of_closures -> t
val value_block : tag * t array -> t
val value_extern : Flambdaexport.ExportId.t -> t
val value_symbol : Symbol.t -> t
val value_bottom : t
val value_unresolved : Symbol.t -> t

val const_approx : Flambda.const -> t

val print_approx : Format.formatter -> t -> unit

val make_const_int : int -> 'a -> 'a Flambda.flambda * t
val make_const_ptr : int -> 'a -> 'a Flambda.flambda * t
val make_const_bool : bool -> 'a -> 'a Flambda.flambda * t
val make_const_float : float -> 'a -> 'a Flambda.flambda * t
val make_const_boxed_int : 'i boxed_int -> 'i -> 'a -> 'a Flambda.flambda * t

(* An approximation is "known" iff it is not [Value_unknown]. *)
val known : t -> bool

(* An approximation is "useful" iff it is neither unknown nor bottom. *)
val useful : t -> bool

(* A value is certainly immutable if its approximation is known and not bottom.
   It should have been resolved (it cannot be [Value_extern] and
   [Value_symbol] *)
val is_certainly_immutable : t -> bool

val check_constant_result
   : Expr_id.t Flambda.flambda
  -> t
  -> Expr_id.t Flambda.flambda * t

val check_var_and_constant_result
   : is_present_in_env:(Variable.t -> bool)
  -> Expr_id.t Flambda.flambda
  -> t
  -> Expr_id.t Flambda.flambda * t

val get_field : int -> t list -> t

val descrs : t list -> descr list

module Import : sig
  val really_import : descr -> descr
  val import_global : Ident.t -> t
  val import_symbol : Symbol.t -> t
end
