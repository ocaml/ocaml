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

(* Translate Lambda code to Flambda code and then optimize it. *)

val middle_end
   : Format.formatter
  -> sourcefile:string
  -> prefixname:string
  -> backend:(module Backend_intf.S)
  -> exported_fields:int
  -> Lambda.lambda
  -> Flambda.t
