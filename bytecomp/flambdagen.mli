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

(* Introduction of closures *)

(* This pass bind free variables of functions in a explicitely created
   closure.

   Also done here:
   * constant blocks are converted to applications of the makeblock
     primitive
   * Levent nodes are removed and their informations is moved to
     raise, function and method calls
   * field(getglobal self) and set_field(getglobal self) are converted
     to the Pgetglobalfield and Psetglobalfield primitives
   * tupled function converted to a stub and a curried function
   * apply and revapply primitives are removed
 *)

open Symbol
open Abstract_identifiers

val intro:
  ?for_bytecode:bool ->
  current_compilation_unit:compilation_unit ->
  current_unit_id:Ident.t ->
  symbol_for_global':(Ident.t -> Symbol.t) ->
  Lambda.lambda ->
  Ident.t VarMap.t * ExprId.t Flambda.flambda
