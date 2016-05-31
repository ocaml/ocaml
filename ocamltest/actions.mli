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

val string_of_result : result -> string

type body = Environments.t -> result

type t = {
  action_name : string;
  action_generated_files : Environments.t -> string list;
  action_body : body
}

val no_generated_files : Environments.t -> string list

val register : string -> (Environments.t -> string list) -> body -> unit

val lookup : string -> t option

val run : Format.formatter -> Environments.t -> t -> result
