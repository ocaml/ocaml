(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Simplification of Flambda programs combined with function inlining:
    for the most part a beta-reduction pass.

    Readers interested in the inlining strategy should read the
    [Inlining_decision] module first.
*)
val run
   : never_inline:bool
  -> backend:(module Backend_intf.S)
  -> prefixname:string
  -> round:int
  -> Flambda.program
  -> Flambda.program
