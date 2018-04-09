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

type exporter = value -> string

type t

val compare : t -> t -> int

exception Empty_variable_name

exception Variable_already_registered

exception No_such_variable of string

val make : string * string -> t

val make_with_exporter : exporter -> string * string -> t

val name_of_variable : t -> string

val description_of_variable : t -> string

val register_variable : t -> unit

val find_variable : string -> t option

val string_of_binding : t -> value -> string
