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

type label = int

type var_location =
  | VL_Register of int
  | VL_Stack of int
  | VL_Optimized_away

type location_range = {
  start_label: label;
  end_label: label;
  location: var_location;
}

type var_info = {
  var_name: string;
  var_reg: Reg.t;
  var_type: Cmm.machtype;
  locations: location_range list;
  is_parameter: bool;
}

type lexical_scope = {
  scope_start: label;
  scope_end: label;
  scope_vars: var_info list;
  nested_scopes: lexical_scope list;
}

type function_var_info = {
  parameters: var_info list;
  root_scope: lexical_scope;
}

let empty_scope ~start ~end_label = {
  scope_start = start;
  scope_end = end_label;
  scope_vars = [];
  nested_scopes = [];
}

let empty_function_info = {
  parameters = [];
  root_scope = empty_scope ~start:0 ~end_label:0;
}

let add_parameter ~name ~reg ~typ ~locations info =
  let param = {
    var_name = name;
    var_reg = reg;
    var_type = typ;
    locations;
    is_parameter = true;
  } in
  { info with parameters = param :: info.parameters }

let add_local_to_scope ~name ~reg ~typ ~locations scope =
  let local = {
    var_name = name;
    var_reg = reg;
    var_type = typ;
    locations;
    is_parameter = false;
  } in
  { scope with scope_vars = local :: scope.scope_vars }

let create_nested_scope ~start ~end_label ~parent =
  let nested = empty_scope ~start ~end_label in
  { parent with nested_scopes = nested :: parent.nested_scopes }

let merge_scope ~child ~parent =
  { parent with nested_scopes = child :: parent.nested_scopes }
