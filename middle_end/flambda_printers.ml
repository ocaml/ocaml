(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let project_closure = Flambda.print_project_closure
let move_within_set_of_closures = Flambda.print_move_within_set_of_closures
let project_var = Flambda.print_project_var
let set_of_closures = Flambda.print_set_of_closures
let function_declaration ppf (var, decl) =
  Flambda.print_function_declaration ppf var decl
let named = Flambda.print_lam_named
let flambda = Flambda.print
