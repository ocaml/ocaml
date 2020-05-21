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

type code = out_channel -> Environments.t -> Result.t * Environments.t

type t

val name : t -> string

val action_name : Variables.t

val update : t -> code -> t

val make : string -> code -> t

val compare : t -> t -> int

val register : t -> unit

val get_registered_actions : unit -> t list

val lookup : string -> t option

val set_hook : string -> code -> unit
val clear_hook : string -> unit
val clear_all_hooks : unit -> unit

val run : out_channel -> Environments.t -> t -> Result.t * Environments.t

module ActionSet : Set.S with type elt = t

module A : sig
  type 'a t = out_channel -> Environments.t -> 'a * Environments.t

  val map: ('a -> 'b) -> 'a t -> 'b t

  val return: 'a -> 'a t

  val if_defined: Variables.t -> 'a t -> 'a t -> 'a t

  val run_cmd:
    environment:string array ->
    stdin_variable:Variables.t ->
    stdout_variable:Variables.t ->
    stderr_variable:Variables.t ->
    append:bool -> string list t -> int t

  val safe_lookup: Variables.t -> string t

  val pair: 'a t -> 'b t -> ('a * 'b) t

  val (let+): 'a t -> ('a -> 'b) -> 'b t
  val (and+): 'a t -> 'b t -> ('a * 'b) t
end
