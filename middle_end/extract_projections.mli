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

(** Identify variables used in function bodies (free variables or
    specialised args, for example) whose approximation says they are
    closures or blocks.  Replace uses of projections from such variables
    with new variables and build a mapping from the new variables to the
    projection expressions. *)

type precondition =
     var:Variable.t
  -> set_of_closures:Flambda.set_of_closures
  -> bool

(** The returned definitions of extracted projection expressions are
    collated together in a list.  Each member of the list corresponds to
    all discovered projections from one particular variable.
    Note that the [Flambda.expr] values here are not rewritten in any
    way: they still reference the variables they did when they were in
    the function body.  Users of this pass may need to perform a
    substitution (see [Unbox_specialised_args] for example).
*)
type projection_defns = Flambda.expr Variable.Map.t list

val from_function_decl
   : precondition:precondition
  -> env:Inline_and_simplify_aux.Env.t
  -> function_decl:Flambda.function_declaration
  -> (Flambda.function_declaration * projection_defns) option
