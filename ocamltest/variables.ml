(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of environment variables *)

type value = string

type exporter = value -> string * string

type t = {
  variable_name : string;
  variable_description : string;
  variable_exporter : exporter
}

let compare v1 v2 = String.compare v1.variable_name v2.variable_name

exception Empty_variable_name

exception Variable_already_registered of string

exception No_such_variable of string

let default_exporter varname value = (varname, value)

let make (name, description) =
  if name="" then raise Empty_variable_name else {
    variable_name = name;
    variable_description = description;
    variable_exporter = default_exporter name
  }

let make_with_exporter exporter (name, description) =
  if name="" then raise Empty_variable_name else {
    variable_name = name;
    variable_description = description;
    variable_exporter = exporter
  }

let name_of_variable v = v.variable_name

let description_of_variable v = v.variable_description

let (variables : (string, t) Hashtbl.t) = Hashtbl.create 10

let register_variable variable =
  if Hashtbl.mem variables variable.variable_name
  then raise (Variable_already_registered variable.variable_name)
  else Hashtbl.add variables variable.variable_name variable

let find_variable variable_name =
  try Some (Hashtbl.find variables variable_name)
  with Not_found -> None

let string_of_binding variable value =
  let (varname, value) = variable.variable_exporter value in
  Printf.sprintf "%s=%s" varname value

let get_registered_variables () =
  let f _variable_name variable variable_list = variable::variable_list in
  List.sort compare (Hashtbl.fold f variables [])
