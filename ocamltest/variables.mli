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

(* The signature of functions that variables support. *)
type var_fun = (string -> string)

type t

val compare : t -> t -> int

exception Empty_variable_name

exception Variable_already_registered of string

exception No_such_variable of string

val make : ?variable_function:var_fun -> string * string -> t

val make_with_exporter :
  ?variable_function:var_fun -> exporter -> string * string -> t

val name_of_variable : t -> string

val description_of_variable : t -> string

val function_of_variable : t -> var_fun option

val register_variable : t -> unit

val find_variable : string -> t option

val string_of_binding : t -> value -> string

val get_registered_variables : unit -> t list
