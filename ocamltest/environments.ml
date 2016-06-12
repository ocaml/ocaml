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

exception Empty_variable_name

exception Empty_environment_name

exception Variable_already_defined of string

exception Environment_already_registered of string

exception Environment_not_found of string

module StringMap = Map.Make (String)

type t = string StringMap.t

let empty = StringMap.empty

let lookup var env =
  try Some (StringMap.find var env) with Not_found -> None

let safe_lookup var env =
  try (StringMap.find var env) with Not_found -> ""

let add_aux variable value env =
  if StringMap.mem variable env then raise (Variable_already_defined variable)
  else StringMap.add variable value env

let add variable value env =
  if variable="" then raise Empty_variable_name
  else add_aux variable value env

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
  StringMap.fold add_aux registered_environment environment

let dump_assignment ppf (variable, value) =
  Format.fprintf ppf "%s = %s" variable value

let dump ppf environment =
  Format.fprintf ppf "@[";
  Format.pp_print_list dump_assignment ppf (StringMap.bindings environment);
  Format.fprintf ppf "@]"
