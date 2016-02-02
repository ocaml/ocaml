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
    specialised args, for example, according to [which_variables] below)
    whose approximation says they are closures or blocks.  Assign "new inner"
    and "new outer" variables with the "new outer" variables carrying the
    relevant information about the projection (cf. [Flambda.specialised_to]).
    Return a map structured as follows:
    - at the top level, indexed by the existing "inner var" being projected
      from;
    - at the second level, indexed by the existing "outer var" being
      projected from, the defining expressions of the projections.  The
      defining expressions are in terms of the existing outer vars.
    The top level of the map is present for the special logic in
    [Augment_specialised_args], a client of this module, which deals with
    adding specialised arguments in groups.  See that module for more details.
*)
type projection_defns = Flambda.named Variable.Map.t Variable.Map.t

type result = {
  (* CR mshinwell: this is a misnomer now.  It is indexed by the
     variables being projected from, then the (existing) outer vars. *)
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

val print_result : Format.formatter -> result -> unit
