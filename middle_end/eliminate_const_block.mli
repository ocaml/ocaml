(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(* Flambda passes expect just one notion of block construction to be used,
   [Pmakeblock], to avoid having to handle [Const_block] throughout as
   well.  This pass expands [Const_block] to [Pmakeblock]. *)

val run : Lambda.lambda -> Lambda.lambda
