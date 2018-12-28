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

let base_type_die_name_for_var compilation_unit var =
  let var_name = Backend_var.name var in
  assert (try ignore (String.index var_name ' '); false
      with Not_found -> true);
  let stamp = Backend_var.stamp var in
  Printf.sprintf "__ocaml %s %s %d"
    (Compilation_unit.string_for_printing compilation_unit)
    var_name stamp

let abstract_instance_root_die_name id =
  let compilation_unit = Debuginfo.Function.Id.compilation_unit id in
  let name =
    Format.asprintf "caml__abstract_instance_%s"
      (Debuginfo.Function.Id.to_string_for_dwarf_die_name id)
  in
  Asm_symbol.of_external_name (DWARF Debug_info) compilation_unit name

let concrete_instance_die_name id =
  let compilation_unit = Debuginfo.Function.Id.compilation_unit id in
  let name =
    Format.asprintf "caml__concrete_instance_%s"
      (Debuginfo.Function.Id.to_string_for_dwarf_die_name id)
  in
  Asm_symbol.of_external_name (DWARF Debug_info) compilation_unit name

let external_declaration_die_name sym compilation_unit =
  let compilation_unit_str =
    Compilation_unit.string_for_printing compilation_unit
  in
  Asm_symbol.prefix sym (DWARF Debug_info) compilation_unit
    ~prefix:("camlDIE__" ^ compilation_unit_str ^ "__")

let ocaml_value_type_name = "__ocaml_value"

let ocaml_naked_float_type_name = "__ocaml_naked_float"

type split_base_type_die_name_result = {
  compilation_unit : string;
  ident_name : string;
  ident_stamp : int;
}

let split_base_type_die_name name =
  match String.split_on_char ' ' name with
  | "__ocaml"::compilation_unit::ident_name::ident_stamp::[] ->
    let ident_stamp = int_of_string ident_stamp in
    Some { compilation_unit; ident_name; ident_stamp; }
  | _ -> None

let mangle_symbol section symbol =
  let unit_name =
    Linkage_name.to_string (Compilation_unit.get_linkage_name (
      Symbol.compilation_unit symbol))
  in
  let symbol' =
    Compilenv.concat_symbol unit_name
      (Linkage_name.to_string (Symbol.label symbol))
  in
  Asm_symbol.of_external_name section (Symbol.compilation_unit symbol) symbol'
