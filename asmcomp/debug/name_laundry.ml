(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let die_name_from_function_name ~function_name =
  "camlDIE__" ^ function_name

let base_type_die_name_for_var var ~output_path =
  let var_name = Backend_var.name var in
  assert (try ignore (String.index var_name ' '); false
      with Not_found -> true);
  let stamp = Backend_var.stamp var in
  Printf.sprintf "__ocaml %s %d %s" var_name stamp output_path

type split_base_type_die_name_result = {
  ident_name : string;
  ident_stamp : int;
  output_path : string;
}

let split_base_type_die_name name =
  match String.split_on_char ' ' name with
  | "__ocaml"::ident_name::ident_stamp::output_path ->
    let output_path = String.concat " " output_path in
    let ident_stamp = int_of_string ident_stamp in
    Some { ident_name; ident_stamp; output_path; }
  | _ -> None
