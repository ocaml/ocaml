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

type body = Environments.t -> result

type t = {
  action_name : string;
  action_body : body
}

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register name body =
  let act = { action_name = name; action_body = body } in
  Hashtbl.add actions name act

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let run env action = action.action_body env
