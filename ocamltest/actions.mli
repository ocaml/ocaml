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

module Eff : sig
  type run_params =
    {
      environment: string array;
      stdin_filename: string;
      stdout_filename: string;
      stderr_filename: string;
      append: bool;
      timeout: int;
      strace: bool;
      strace_logfile: string;
      strace_flags: string;
    }

  val default_params: run_params

  val run_cmd: run_params -> string list -> int

  (* ?environment:string array t -> *)
  (* ?stdin_variable:Variables.t -> *)
  (* ?stdout_variable:Variables.t -> *)
  (* ?stderr_variable:Variables.t -> *)
  (* ?append:bool -> string list t -> int t *)

  val setup_symlinks: string -> string -> string list -> unit

  val force_remove: string -> unit
end

module A : sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t

  val return: 'a -> 'a t

  val apply: ('a -> 'b) t -> 'a t -> 'b t

  val if_: bool t -> 'a t -> 'a t -> 'a t

  val select: ('a, 'b) Stdlib.Result.t t -> ('a -> 'b) t -> 'b t

  val safe_lookup: Variables.t -> string t
  val lookup: Variables.t -> string option t
  val lookup_nonempty: Variables.t -> string option t
  val lookup_as_bool: Variables.t -> bool option t

  val add_if_undefined: Variables.t -> string t -> 'a t -> 'a t
  val add: Variables.t -> string t -> 'a t -> 'a t
  val with_env: 'a t -> ('a * Environments.t) t

  val file_exists: string t -> bool t

  val concatmap: ('a -> 'b list t) -> 'a list t -> 'b list t

  val while_: ('a -> ('x, 'b) Stdlib.Result.t t) -> 'x -> 'a list t -> ('x, 'b) Stdlib.Result.t t

  val system_env: string array t

  val apply_modifiers: Environments.modifiers -> 'a t -> 'a t

  val branch: ('a, 'b) Stdlib.Result.t t -> ('a -> 'c) t -> ('b -> 'c) t -> 'c t

  val cast: Environments.t t -> (out_channel -> Environments.t -> Environments.t)

  module Infix : sig
    val (let+): 'a t -> ('a -> 'b) -> 'b t
    val (and+): 'a t -> 'b t -> ('a * 'b) t
    val (||+): bool t -> bool t -> bool t
    val (&&+): bool t -> bool t -> bool t
  end
end

type code = (Result.t * Environments.t) A.t

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
