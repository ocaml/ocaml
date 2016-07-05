(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of environments, used to pass parameters to tests and actions *)

exception Empty_environment_name

exception Variable_already_defined of Variables.t

exception Environment_already_registered of string

exception Environment_not_found of string

module VariableMap = Map.Make (Variables)

type t = string VariableMap.t

let empty = VariableMap.empty

let lookup variable env =
  try Some (VariableMap.find variable env) with Not_found -> None

let safe_lookup variable env =
  try (VariableMap.find variable env) with Not_found -> ""

let is_variable_defined variable env =
  VariableMap.mem variable env

let add variable value env =
  if VariableMap.mem variable env
  then raise (Variable_already_defined variable)
  else VariableMap.add variable value env

let add_bindings bindings env =
  let f env (variable, value) = add variable value env in
  List.fold_left f env bindings

let from_bindings bindings = add_bindings bindings empty

let (registered_environments : (string, t) Hashtbl.t) = Hashtbl.create 20

let register environment_name environment =
  if environment_name="" then raise Empty_environment_name
  else if Hashtbl.mem registered_environments environment_name
  then raise (Environment_already_registered environment_name)
  else Hashtbl.add registered_environments environment_name environment

let find_environemnt environment_name =
  try Hashtbl.find registered_environments environment_name
  with Not_found -> raise (Environment_not_found environment_name)

let include_ environment_name environment =
  let registered_environment = find_environemnt environment_name in
  VariableMap.fold add registered_environment environment

let dump_assignment log (variable, value) =
  Printf.fprintf log "%s = %s\n%!" (Variables.name_of_variable variable) value

let dump log environment =
  List.iter (dump_assignment log) (VariableMap.bindings environment);
