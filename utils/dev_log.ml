(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compiler_diagnostic.Dev
let slist = Diagnostic.(List String)
let v1 = Compiler_diagnostic.v1

let parsetree = new_field_opt v1 "parsetree" String
let source = new_field_opt v1 "source" String
let typedtree = new_field_opt v1 "typedtree" String
let shape = new_field_opt v1 "shape" String
let instr = new_field_opt v1 "instr" String
let lambda = new_field_opt v1 "lambda" String
let raw_lambda = new_field_opt v1 "rawlambda" String
let flambda = new_field_opt v1 "flambda" slist
let raw_flambda = new_field_opt v1 "rawflambda" slist
let clambda = new_field_opt v1 "clambda" slist
let raw_clambda = new_field_opt v1 "raw_clambda" slist
let cmm = new_field_opt v1 "cmm" slist
let remove_free_vars_equal_to_args =
  new_field_opt v1 "remove_free_vars_equal_to_args" slist
let unbox_free_vars_of_closures =
  new_field_opt v1 "unbox_free_vars_of_closures" slist
let unbox_closures = new_field_opt v1 "unbox_closures" slist
let unbox_specialised_args = new_field_opt v1 "unbox_specialised_args" slist
let mach = new_field_opt v1 "mach" slist
let linear = new_field_opt v1 "linear" slist
let cmm_invariant = new_field_opt v1 "cmm_invariant" String

type t = id Log.t
