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

(** Variable information preservation for DWARF debugging.

    This module provides a side table to preserve variable names and types
    from the Cmm IR level through to emission, where they can be used to
    generate DWARF debug information.

    The challenge: Variable names exist in Cmm (Backend_var.With_provenance.t)
    but are lost during conversion to Mach IR (Reg.t). This module bridges
    that gap by storing the name information separately and making it available
    during emission. *)

(** Variable information for a single variable *)
type var_info = {
  name : string;                    (* Source-level variable name *)
  reg_index : int;                  (* Position in parameter array *)
  is_parameter : bool;              (* true for parameters, false for locals *)
}

(** Function's variable information *)
type function_info = {
  fun_name : string;
  parameters : var_info list;
  (* Future: add locals *)
}

(** Clear all stored variable information (call at start of compilation unit) *)
val reset : unit -> unit

(** Record parameter information for a function during selection phase *)
val record_function_parameters :
  fun_name:string ->
  param_names:string list ->
  unit

(** Look up variable information for a function during emission *)
val get_function_info : string -> function_info option

(** Get parameter name by index, returns empty string if not found *)
val get_parameter_name : fun_name:string -> index:int -> string
