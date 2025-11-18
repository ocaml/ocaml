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

(** Variable tracking for DWARF debug information.

    This module implements the data structures needed to track variables
    through their lifetimes, including lexical scopes and location changes.
    This is the proper implementation of DWARF_LOCAL_VARIABLES_PLAN.md. *)

type label = int

(** Variable location at a specific point *)
type var_location =
  | VL_Register of int                (* In register *)
  | VL_Stack of int                   (* Stack offset *)
  | VL_Optimized_away                 (* Optimized out *)

(** A variable's location over a range of code *)
type location_range = {
  start_label: label;                 (* First instruction label *)
  end_label: label;                   (* Last instruction label *)
  location: var_location;             (* Where variable lives *)
}

(** Variable information for DWARF emission *)
type var_info = {
  var_name: string;                   (* Source-level name *)
  var_reg: Reg.t;                     (* Associated register *)
  var_type: Cmm.machtype;             (* OCaml type *)
  locations: location_range list;     (* Location list *)
  is_parameter: bool;                 (* Parameter vs local *)
}

(** Lexical scope containing variables *)
type lexical_scope = {
  scope_start: label;                 (* Scope entry point *)
  scope_end: label;                   (* Scope exit point *)
  scope_vars: var_info list;          (* Variables in scope *)
  nested_scopes: lexical_scope list;  (* Nested scopes *)
}

(** Variable tracking info for a function *)
type function_var_info = {
  parameters: var_info list;          (* Function parameters *)
  root_scope: lexical_scope;          (* Root lexical scope *)
}

(** Create empty function variable info *)
val empty_function_info : function_var_info

(** Add a parameter to function info *)
val add_parameter :
  name:string ->
  reg:Reg.t ->
  typ:Cmm.machtype ->
  locations:location_range list ->
  function_var_info ->
  function_var_info

(** Add a local variable to a scope *)
val add_local_to_scope :
  name:string ->
  reg:Reg.t ->
  typ:Cmm.machtype ->
  locations:location_range list ->
  lexical_scope ->
  lexical_scope

(** Create a new nested scope *)
val create_nested_scope :
  start:label ->
  end_label:label ->
  parent:lexical_scope ->
  lexical_scope

(** Merge a nested scope into its parent *)
val merge_scope :
  child:lexical_scope ->
  parent:lexical_scope ->
  lexical_scope
