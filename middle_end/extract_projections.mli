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
    with new "inner" variables, each associated with a new "outer" variable,
    and build a mapping from the new outer variables to the projection
    expressions (rewritten to use the new outer variables).  The benefit
    of removing the projections is also returned.

    The returned definitions of extracted projection expressions are
    collated together in a list.  Each member of the list corresponds to
    all discovered projections from one particular variable.
*)
type projection_defns = Flambda.expr Variable.Map.t list

type result = {
  projection_defns_indexed_by_outer_vars : projection_defns;
  new_function_body : Flambda.expr;
  new_inner_to_new_outer_vars : Variable.t Variable.Map.t;
  benefit : Inlining_cost.Benefit.t;
}

(** [which_variables] maps inner variables to outer variables in the
    manner of [free_vars] and [specialised_args] in
    [Flambda.set_of_closures]. *)
val from_function_decl
   : which_variables:Variable.t Variable.Map.t
  -> env:Inline_and_simplify_aux.Env.t
  -> function_decl:Flambda.function_declaration
  -> result
