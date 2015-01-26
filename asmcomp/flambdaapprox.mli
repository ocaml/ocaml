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

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_set_of_closures of value_closure
  | Value_closure of value_offset
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t

and value_offset = {
  fun_id : Closure_id.t;
  closure : value_closure;
}

and value_closure = {
  ffunctions : Expr_id.t function_declarations;
  bound_var : approx Var_within_closure.Map.t;
  kept_params : Variable.Set.t;
  ffunction_sb :
    Flambdasubst.Alpha_renaming_map_for_ids_and_bound_vars_of_closures.t;
}

(* CR mshinwell for pchambart: call this type [t] *)
and approx = {
  descr : descr;
  var : Variable.t option;
  symbol : Symbol.t option;
}
(** A value of type [approx] corresponds to an approximation of a value.
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

val value_unknown : approx
val value_int : int -> approx
val value_constptr : int -> approx
val value_closure : value_offset -> approx
val value_unoffseted_closure : value_closure -> approx
val value_block : tag * approx array -> approx
val value_extern : Flambdaexport.ExportId.t -> approx
val value_symbol : Symbol.t -> approx
val value_bottom : approx

val const_approx : Flambda.const -> approx

val print_approx : Format.formatter -> approx -> unit

val make_const_int : int -> 'a -> 'a Flambda.flambda * approx
val make_const_ptr : int -> 'a -> 'a Flambda.flambda * approx
val make_const_bool : bool -> 'a -> 'a Flambda.flambda * approx

(* An approximation is "useful" iff it is neither unknown nor bottom. *)
val useful : approx -> bool

val check_constant_result
   : Expr_id.t Flambda.flambda
  -> approx
  -> Expr_id.t Flambda.flambda * approx

val check_var_and_constant_result
   : is_present_in_env:(Variable.t -> bool)
  -> Expr_id.t Flambda.flambda
  -> approx
  -> Expr_id.t Flambda.flambda * approx

val get_field : int -> approx list -> approx

val descrs : approx list -> descr list

module Import : sig
  val really_import : descr -> descr
  val import_global : Ident.t -> approx
  val import_symbol : Symbol.t -> approx
end
