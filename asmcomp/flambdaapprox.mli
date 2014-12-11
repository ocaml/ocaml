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

type tag = int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_unoffseted_closure of value_closure
  | Value_closure of value_offset
  | Value_unknown
  | Value_bottom
  | Value_extern of Flambdaexport.ExportId.t
  | Value_symbol of Symbol.t

(* TODO: rename *)
and value_offset =
  { fun_id : function_within_closure;
    closure : value_closure }

and value_closure =
  { ffunctions : ExprId.t function_declarations;
    bound_var : approx ClosureVariableMap.t;
    kept_params : VarSet.t;
    fv_subst_renaming : variable_within_closure ClosureVariableMap.t;
    fun_subst_renaming : function_within_closure ClosureFunctionMap.t }

and approx =
  { descr : descr;
    var : Variable.t option;
    (* Highest bound variable containing *)
    symbol : Symbol.t option }

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

module Import : sig
  val really_import : descr -> descr
  val import_global : Ident.t -> approx
  val import_symbol : Symbol.t -> approx
end
