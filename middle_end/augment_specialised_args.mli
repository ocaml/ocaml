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

type new_specialised_arg = {
  definition : Flambda.expr;
  (** [definition], if referencing one of the parameters of the function,
      must use the variables in the range of
      [names_of_params_to_use_in_definitions] (see below) rather than the
      original parameter variables of the function. *)
}

(** This maps from new names (chosen by the client of this module) used
    inside the rewritten function body. *)
type add_all_or_none_of_these_specialised_args =
  new_specialised_arg Variable.Map.t

type what_to_specialise = {
  new_function_body : Flambda.expr;
  new_specialised_args : add_all_or_none_of_these_specialised_args list;
}

module type S = sig
  val pass_name : string

  val precondition : set_of_closures:Flambda.set_of_closures -> bool

  val what_to_specialise
     : names_of_params_to_use_in_definitions:Variable.t Variable.Map.t
    -> closure_id:Closure_id.t
    -> function_decl:Flambda.function_declaration
    -> set_of_closures:Flambda.set_of_closures
    -> what_to_specialise option
end

module Make (T : S) : sig
  val rewrite_set_of_closures
     : backend:(module Backend_intf.S)
    -> set_of_closures:Flambda.set_of_closures
    -> Flambda.set_of_closures option
end
