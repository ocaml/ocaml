(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

(** Helper module for adding specialised arguments to sets of closures. *)

(** This maps from new names (chosen by the client of this module) used
    inside the rewritten function body and which will form the augmented
    specialised argument list on the main function.
    The domain of the map is definitions of the new specialised args.
    Such definitions, if referencing specialised args of the function,
    must use the "outer variables" in the range of the specialised
    argument map in the set of closures rather than the current parameters
    of the function (which are the "inner variables", the domain of that
    map).
    There is no support yet for these [definition]s being dependent on
    each other in any way. *)
type add_all_or_none_of_these_specialised_args =
  Flambda.expr Variable.Map.t

type what_to_specialise = {
  new_function_body : Flambda.expr;
  new_specialised_args : add_all_or_none_of_these_specialised_args list;
  total_benefit : Inlining_cost.Benefit.t;
}

module type S = sig
  val pass_name : string
  val variable_suffix : string

  val precondition : set_of_closures:Flambda.set_of_closures -> bool

  val what_to_specialise
     : env:Inline_and_simplify_aux.Env.t
    -> closure_id:Closure_id.t
    -> function_decl:Flambda.function_declaration
    -> set_of_closures:Flambda.set_of_closures
    -> what_to_specialise option
end

module type Result = sig
  val rewrite_set_of_closures
     : backend:(module Backend_intf.S)
    -> env:Inline_and_simplify_aux.Env.t
    -> set_of_closures:Flambda.set_of_closures
    -> (Flambda.expr * Inlining_cost.Benefit.t) option
end

module Make (T : S) : Result
