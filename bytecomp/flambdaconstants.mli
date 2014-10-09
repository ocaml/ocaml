(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Symbol
open Abstract_identifiers

type constant_result = {
  not_constant_id : VarSet.t;
  not_constant_closure : FunSet.t;
}

val not_constants :
  for_clambda:bool -> compilation_unit:compilation_unit ->
  'a Flambda.flambda -> constant_result
(** [not_constant ~for_clambda expr]
    If for_clambda is true, are marked constant only expressions that can
    effectively be compiled to constants by Clambdagen.
    When for_clambda is false, field access to a constant are not considered
    constant *)
