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
    closures.  Replace the uses with new variables and build a mapping
    from the new variables to the extracted projections.  The mapping is
    then used by a provided function to rebuild the set of closures. *)

val from_set_of_closures
   : precondition:(var:Variable.t
      -> set_of_closures:Flambda.set_of_closures
      -> bool)
  -> env:Inline_and_simplify_aux.Env.t
  -> set_of_closures:Flambda.set_of_closures
  -> rebuild_function_decl:(function_body:Flambda.expr -> ...)
  -> Flambda.set_of_closures
