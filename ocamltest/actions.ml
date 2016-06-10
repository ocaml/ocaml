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

(* Definition of actions, basic blocks for tests *)

type result =
  | Pass of Environments.t
  | Fail of string
  | Skip of string

let string_of_reason prefix reason =
  if reason="" then prefix
  else prefix ^ " (" ^ reason ^ ")"

let string_of_result = function
  | Pass _ -> "Pass"
  | Fail reason -> string_of_reason "Fail" reason
  | Skip reason -> string_of_reason "Skip" reason

type body = Format.formatter -> Environments.t -> result

type t = {
  action_name : string;
  action_environment : Environments.t -> Environments.t;
  action_generated_files : Environments.t -> string list;
  action_body : body
}

let compare a1 a2 = String.compare a1.action_name a2.action_name

let no_generated_files env = []

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register action =
  Hashtbl.add actions action.action_name action

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let update_environment initial_env actions =
  let f env act = act.action_environment env in
  List.fold_left f initial_env actions

let run ppf env action =
  let files = action.action_generated_files env in
  Format.fprintf ppf "Generated files:\n";
  List.iter (Format.fprintf ppf "%s\n") files;
  action.action_body ppf env
