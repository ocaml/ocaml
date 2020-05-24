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

    (** The result of executing a side effect *)

    type status = Pass | Skip | Fail

    type t = {
      status : status;
      reason : string option
    }

    val pass : t
    val skip : t
    val fail : t

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
    }

  val run_cmd: run_params -> string list -> t
  (** Execute a command. *)

  val setup_symlinks: string -> string -> string list -> t

  val force_remove: string -> t
  (** Remove a file. *)

  val pass: t
  (** A side effects that does nothing (and succeeds) *)

  val skip: t
  (** A side effects that does nothing (and skips) *)

  val fail: t
  (** A side effects that does nothing (and fails) *)

  val pass_with_reason: string -> t
  val skip_with_reason: string -> t
  val fail_with_reason: string -> t

  val with_exit_code: int -> t -> t
  (** [with_exit_code n t] is a side effect that does [t] and *passes* if [t]
      completes with exit code [n]. *)

  val with_skip_code: int -> t -> t
  (** [with_skip_code n t] is a side effect that does [t] and *skips* if [t]
      completes with exit code [n]. *)

  val chdir: string -> t
  (** Change the working directory. *)

  val check_files:
    kind_of_output:string ->
    promote:bool option -> Filecompare.ignore -> Filecompare.files -> t

  val compare_files:
    tool:Filecompare.tool -> Filecompare.files -> t

  val seq: t list -> t
  (** Perform a series of effects in sequence. Stops at the first
      failure/skip. *)

  val if_pass: t -> t -> t
  (** [if_pass a b] performs [a] and, if it passes, then it performs [b]. *)

  val echo: ('a, unit, string, t) format4 -> 'a
  (** Output to the log. *)

  val run: t -> dry_run:bool -> out_channel -> Result.t
  (** Actually execute a side-effect. If [dry_run] is [true], then no actual
      action is performed. *)
end

module A : sig

  (** The action functor *)

  type 'a t
  (** This represents an action that produces a value of type ['a]. *)

  val map: ('a -> 'b) -> 'a t -> 'b t
  (** Applicative map. *)

  val return: 'a -> 'a t
  (** The constant action. *)

  val if_: bool t -> 'a t -> 'a t -> 'a t
  (** [if_ a b c ] is [b] or [c], depending on the value produced by [a]. *)

  val safe_lookup: Variables.t -> string t
  (** Lookup a variable. Return [""] if not defined. *)

  val lookup: Variables.t -> string option t
  (** Lookup a variable. *)

  val lookup_nonempty: Variables.t -> string option t
  (** Lookup a variable. Return [None] if the variable is empty. *)

  val lookup_as_bool: Variables.t -> bool option t
  (** Lookup a variable, return [true] or [false] if its value is ["true"] or
      ["false"]. *)

  val add_if_undefined: Variables.t -> string t -> 'a t -> 'a t
  (** Add a variable to the environment, but only if it is not already
      defined. *)

  val add: Variables.t -> string t -> 'a t -> 'a t
  (** Add/update a variable in the environment. *)

  val env: Environments.t t
  (** The current environment. *)

  val both: 'a t -> 'b t -> ('a * 'b) t
  (** Applicative pair. *)

  val all: 'a t list -> 'a list t
  (** Applicative list. *)

  val apply_modifiers: Environments.modifiers -> 'a t -> 'a t

  val run: 'a t -> Environments.t -> 'a
  (** [run a env] "executes" an action in an environment [env] and returns its
      result. *)

  module Infix : sig
    val (let+): 'a t -> ('a -> 'b) -> 'b t
    val (and+): 'a t -> 'b t -> ('a * 'b) t
    val (||+): bool t -> bool t -> bool t
    val (&&+): bool t -> bool t -> bool t
  end

  module Uses : sig
    (** Static analysis of actions *)

    val reads: 'a t -> Variables.Set.t
    (** Variables read by the action. *)

    val writes: 'a t -> Variables.Set.t
    (** Variables written to by the action. *)
  end
end

type code = (Eff.t * Environments.t) A.t
(** A [code] is a top-level action: ie an action that produces a description of
    the computation to do, and an updated environment. *)

type t

val name : t -> string

val action_name : Variables.t

val update : t -> code -> t

val make : string -> Eff.t A.t -> t

val make_env : string -> (Eff.t * Environments.t) A.t -> t

val body : t -> code

val compare : t -> t -> int

val register : t -> unit

val get_registered_actions : unit -> t list

val lookup : string -> t option

val set_hook : string -> code -> unit
val clear_hook : string -> unit
val clear_all_hooks : unit -> unit

val run : Environments.t -> t -> Eff.t * Environments.t

module ActionSet : Set.S with type elt = t
