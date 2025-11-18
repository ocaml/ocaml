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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type var_info = {
  name : string;
  reg_index : int;
  is_parameter : bool;
}

type function_info = {
  fun_name : string;
  parameters : var_info list;
}

(* Global table mapping function names to their variable information *)
let function_table : (string, function_info) Hashtbl.t =
  Hashtbl.create 100

let reset () =
  Hashtbl.clear function_table

let record_function_parameters ~fun_name ~param_names =
  let parameters = List.mapi (fun index name ->
    {
      name;
      reg_index = index;
      is_parameter = true;
    }
  ) param_names in
  let info = { fun_name; parameters } in
  Hashtbl.replace function_table fun_name info

let get_function_info fun_name =
  Hashtbl.find_opt function_table fun_name

let get_parameter_name ~fun_name ~index =
  match get_function_info fun_name with
  | None -> ""
  | Some info ->
      match List.find_opt (fun var -> var.reg_index = index) info.parameters with
      | None -> ""
      | Some var -> var.name
