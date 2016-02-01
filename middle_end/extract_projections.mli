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

(* CR mshinwell: comment out of date *)
(** Identify variables used in function bodies (free variables or
    specialised args, for example) whose approximation says they are
    closures or blocks.  Arrange (by annotations in [specialised_args]) for
    uses of projections from such variables to be replaced (by
    [Inline_and_simplify]) with new "inner" variables, each associated with
    a new "outer" variable, and build a mapping from the new outer variables
    to the projection expressions (rewritten to use the new outer variables).

    The returned definitions of extracted projection expressions are
    collated together in a list.  Each member of the list corresponds to
    all discovered projections from one particular variable.
*)
(* First level: the original (inner spec arg) variable being projected from
   Second level: new outer var
   Result: projection defining expr in terms of original outer vars *)
type projection_defns = Flambda.named Variable.Map.t Variable.Map.t

type result = {
  (* CR mshinwell: this is a misnomer now.
     Now, the maps are:
        - first, a map from the variables being projected from;
        - second, a map from the "new outer vars" to the defining expressions
          of the projections. *)
  projection_defns_indexed_by_outer_vars : projection_defns;
  new_inner_to_new_outer_vars : Flambda.specialised_to Variable.Map.t;
}

(** [which_variables] maps inner variables to outer variables in the
    manner of [free_vars] and [specialised_args] in
    [Flambda.set_of_closures]. *)
val from_function_decl
   : which_variables:Flambda.specialised_to Variable.Map.t
  -> env:Inline_and_simplify_aux.Env.t
  -> function_decl:Flambda.function_declaration
  -> result option
