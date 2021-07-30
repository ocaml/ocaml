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

type t

val empty : t

val from_bindings : (Variables.t * string) list -> t
val to_bindings : t -> (Variables.t * string) list
val to_system_env : t -> string array
val append_to_system_env : string array -> t -> string array

val lookup : Variables.t -> t -> string option
val lookup_nonempty : Variables.t -> t -> string option
val safe_lookup : Variables.t -> t -> string
val is_variable_defined : Variables.t -> t -> bool

val lookup_as_bool : Variables.t -> t -> bool option
(** returns [Some true] if the variable is set to ["true"],
    [Some false] if it is set to another string, and
    [None] if not set. *)

val lookup_as_int : Variables.t -> t -> int option
(** returns [Some n] if the variable is set to a string
    representation of the integer [n],
    and [None] if it is not an integer or not set. *)

val add : Variables.t -> string -> t -> t
val add_if_undefined : Variables.t -> string -> t -> t
val add_bindings : (Variables.t * string) list -> t -> t

val unsetenv : Variables.t -> t -> t
(** [unsetenv env name] causes [name] to be ignored from the underlying system
    environment *)

val append : Variables.t -> string -> t -> t

val dump : out_channel -> t -> unit

(* Initializers *)

type kind = Pre | Post

type env_initializer = out_channel -> t -> t

val register_initializer : kind -> string -> env_initializer -> unit

val initialize : kind -> env_initializer

(* Modifiers *)

type modifier =
  | Include of string
  | Add of Variables.t * string
  | Append of Variables.t * string
  | Remove of Variables.t

type modifiers = modifier list

val apply_modifier : t -> modifier -> t
val apply_modifiers : t -> modifiers -> t

exception Empty_modifiers_name
exception Modifiers_name_already_registered of string
exception Modifiers_name_not_found of string

val register_modifiers : string -> modifiers -> unit
