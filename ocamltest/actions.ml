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

type code = out_channel -> Environments.t -> result

type t = {
  name : string;
  body : code;
  mutable hook : code option
}

let action_name a = a.name

let make n c = { name = n; body = c; hook = None }

let compare a1 a2 = String.compare a1.name a2.name

let (actions : (string, t) Hashtbl.t) = Hashtbl.create 10

let register action =
  Hashtbl.add actions action.name action

let get_registered_actions () =
  let f _name action acc = action::acc in
  let unsorted_actions = Hashtbl.fold f actions [] in
  List.sort compare unsorted_actions

let lookup name =
  try Some (Hashtbl.find actions name)
  with Not_found -> None

let set_hook name hook =
  let action = (Hashtbl.find actions name) in
  action.hook <- Some hook

let clear_hook name =
  let action = (Hashtbl.find actions name) in
  action.hook <- None

let clear_all_hooks () =
  let f _name action = action.hook <- None in
  Hashtbl.iter f actions

let run log env action =
  let code = match action.hook with
    | None -> action.body
    | Some code -> code in
  code log env

module ActionSet = Set.Make
(struct
  type nonrec t = t
  let compare = compare
end)
