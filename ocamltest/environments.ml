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

(* Definition of environments, used to pass parameters to tests and actions *)

open Ocamltest_stdlib

module VariableMap = Map.Make (Variables)

type t = string option VariableMap.t

let empty = VariableMap.empty

let to_bindings env =
  let f variable value lst =
    Option.fold ~none:lst ~some:(fun value -> (variable, value) :: lst) value
  in
  VariableMap.fold f env []

let expand_aux env value =
  let bindings = to_bindings env in
  let f (variable, value) = ((Variables.name_of_variable variable), value) in
  let simple_bindings = List.map f bindings in
  let subst s = try (List.assoc s simple_bindings) with Not_found -> "" in
  let b = Buffer.create 100 in
  try Buffer.add_substitute b subst value; Buffer.contents b with _ -> value

let rec expand env value =
  let expanded = expand_aux env value in
  if expanded=value then value else expand env expanded

let expand env = function
  | None -> raise Not_found
  | Some value -> expand env value

let append_to_system_env environment env =
  (* Augment env with any bindings which are only in environment. This must be
     done here as the Windows C implementation doesn't process multiple values
     in settings.envp. *)
  let env =
    let update env binding =
      let name, value =
        match String.index binding '=' with
        | c ->
            let name = String.sub binding 0 c in
            let value =
              String.sub binding (c + 1) (String.length binding - c - 1) in
            (name, Some value)
        | exception Not_found ->
            (binding, None)
      in
      let var = Variables.make (name, "system env var") in
        if not (VariableMap.mem var env) then
          VariableMap.add var value env
        else
          env
    in
      Array.fold_left update env environment
  in
  let system_env = Array.make (VariableMap.cardinal env) "" in
  let i = ref 0 in
  let store variable value =
    let some value =
      Variables.string_of_binding variable (expand env (Some value)) in
    system_env.(!i) <-
      Option.fold ~none:(Variables.name_of_variable variable) ~some value;
    incr i in
  VariableMap.iter store env;
  system_env

let to_system_env env =
  append_to_system_env [||] env

let lookup variable env =
  try Some (expand env (VariableMap.find variable env)) with Not_found -> None

let lookup_nonempty variable env = match lookup variable env with
  | None -> None
  | Some x as t -> if String.words x = [] then None else t

let lookup_as_bool variable env =
  match lookup variable env with
  | None -> None
  | Some "true" -> Some true
  | Some _ -> Some false

let lookup_as_int variable env =
  match lookup variable env with
  | None -> None
  | Some value ->
      int_of_string_opt value

let safe_lookup variable env = match lookup variable env with
  | None -> ""
  | Some value -> value

let is_variable_defined variable env =
  VariableMap.mem variable env

let add variable value env = VariableMap.add variable (Some value) env

let add_if_undefined variable value env =
  if VariableMap.mem variable env then env else add variable value env

let append variable appened_value environment =
  let previous_value = safe_lookup variable environment in
  let new_value = previous_value ^ appened_value in
  VariableMap.add variable (Some new_value) environment

let remove = VariableMap.remove

let unsetenv variable environment =
  VariableMap.add variable None environment

let add_bindings bindings env =
  let f env (variable, value) = add variable value env in
  List.fold_left f env bindings

let from_bindings bindings = add_bindings bindings empty

let dump_assignment log = function
  | (variable, Some value) ->
    Printf.fprintf log "%s = %s\n%!" (Variables.name_of_variable variable) value
  | (variable, None) ->
    Printf.fprintf log "unsetenv %s\n%!" (Variables.name_of_variable variable)

let dump log environment =
  List.iter (dump_assignment log) (VariableMap.bindings environment)

(* Initializers *)

type kind = Pre | Post

type env_initializer = out_channel -> t -> t

type initializers =
  {
    pre: (string, env_initializer) Hashtbl.t;
    post: (string, env_initializer) Hashtbl.t;
  }

let initializers = {pre = Hashtbl.create 10; post = Hashtbl.create 10}

let get_initializers = function
  | Pre -> initializers.pre
  | Post -> initializers.post

let register_initializer kind name code =
  Hashtbl.add (get_initializers kind) name code

let apply_initializer _log _name code env =
  code _log env

let initialize kind log env =
  let f = apply_initializer log in
  Hashtbl.fold f (get_initializers kind) env

(* Modifiers *)

type modifier =
  | Include of string
  | Add of Variables.t * string
  | Append of Variables.t * string
  | Remove of Variables.t

type modifiers = modifier list

exception Empty_modifiers_name
exception Modifiers_name_already_registered of string
exception Modifiers_name_not_found of string

let (registered_modifiers : (string, modifiers) Hashtbl.t) = Hashtbl.create 20

let register_modifiers name modifiers =
  if name="" then raise Empty_modifiers_name
  else if Hashtbl.mem registered_modifiers name
  then raise (Modifiers_name_already_registered name)
  else Hashtbl.add registered_modifiers name modifiers

let find_modifiers name =
  try Hashtbl.find registered_modifiers name
  with Not_found -> raise (Modifiers_name_not_found name)

let rec apply_modifier environment = function
  | Include modifiers_name ->
    apply_modifiers environment (find_modifiers modifiers_name)
  | Add (variable, value) -> add variable value environment
  | Append (variable, value) -> append variable value environment
  | Remove variable -> remove variable environment
and apply_modifiers environment modifiers =
  List.fold_left apply_modifier environment modifiers
