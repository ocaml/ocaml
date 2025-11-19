(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Joel Reymont, Claude AI Assistant                          *)
(*                                                                        *)
(*   Copyright 2025                                                       *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variable lifetime tracking during instruction selection.

    This module tracks variables as they are bound and used during
    selection, computing their lifetimes and building the variable
    tracking information for DWARF emission. *)

type tracker

(** Create a new tracker for a function *)
val create : fun_name:string -> tracker

(** Record a parameter binding *)
val add_parameter :
  tracker ->
  name:string ->
  reg:Reg.t ->
  typ:Cmm.machtype ->
  unit

(** Record a local variable binding *)
val add_local :
  tracker ->
  name:string ->
  reg:Reg.t ->
  typ:Cmm.machtype ->
  unit

(** Enter a new lexical scope *)
val enter_scope : tracker -> unit

(** Exit the current lexical scope *)
val exit_scope : tracker -> unit

(** Build the final function_var_info *)
val finalize : tracker -> Var_tracking.function_var_info

(** Update location information after register allocation.

    This should be called after register allocation has completed,
    when all Reg.t values have their actual locations assigned.
    It walks through the function_var_info and updates each
    location_range with the actual location from the register. *)
val update_locations :
  Var_tracking.function_var_info ->
  Var_tracking.function_var_info
