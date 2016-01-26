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

(** Turn free variables of closures into specialised arguments.
    The aim is to cause the closure to become closed. *)

val rewrite_set_of_closures
   : backend:(module Backend_intf.S)
  -> env:Inline_and_simplify_aux.Env.t
  -> set_of_closures:Flambda.set_of_closures
  -> (Flambda.expr * Inlining_cost.Benefit.t) option
