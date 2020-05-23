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

  module Result : sig
    type status = Pass | Skip | Fail

    type t = {
      status : status;
      reason : string option
    }

    val pass : t

    val skip : t

    val fail : t

    val pass_with_reason : string -> t

    val skip_with_reason : string -> t

    val fail_with_reason : string -> t

    val string_of_result : t -> string

    val is_pass : t -> bool

    val is_skip : t -> bool

    val is_fail : t -> bool
  end

  type t

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
      expected_exit_codes: int list;
      skip_exit_codes: int list;
      (* reason: string; *)
    }

  val run_cmd: run_params -> string list -> t

  val setup_symlinks: string -> string -> string list -> t

  val force_remove: string -> t

  val pass: t
  val skip: t
  val fail: t
  val pass_with_reason: string -> t
  val skip_with_reason: string -> t
  val fail_with_reason: string -> t

  val cd: string -> t

  val check_files:
    kind_of_output:string ->
    promote:bool option -> Filecompare.ignore -> Filecompare.files -> t

  val compare_files:
    tool:Filecompare.tool -> Filecompare.files -> t

  val seq: t list -> t

  val if_pass: t -> t -> t

  val echo: ('a, unit, string, t) format4 -> 'a
end

module A : sig
  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t

  val return: 'a -> 'a t

  val if_: bool t -> 'a t -> 'a t -> 'a t

  val safe_lookup: Variables.t -> string t
  val lookup: Variables.t -> string option t
  val lookup_nonempty: Variables.t -> string option t
  val lookup_as_bool: Variables.t -> bool option t

  val add_if_undefined: Variables.t -> string t -> 'a t -> 'a t
  val add: Variables.t -> string t -> 'a t -> 'a t
  val env: Environments.t t
  val both: 'a t -> 'b t -> ('a * 'b) t

  val file_exists: string t -> bool t

  val all: 'a t list -> 'a list t

  val apply_modifiers: Environments.modifiers -> 'a t -> 'a t

  val cast: Environments.t t -> (out_channel -> Environments.t -> Environments.t)

  module Infix : sig
    val (let+): 'a t -> ('a -> 'b) -> 'b t
    val (and+): 'a t -> 'b t -> ('a * 'b) t
    val (||+): bool t -> bool t -> bool t
    val (&&+): bool t -> bool t -> bool t
  end
end

type code = (Eff.t * Environments.t) A.t

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

val run : out_channel -> Environments.t -> t -> Eff.Result.t * Environments.t

module ActionSet : Set.S with type elt = t
