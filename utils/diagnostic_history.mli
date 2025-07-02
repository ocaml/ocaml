(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


 (** {1:version  Versions for structured diagnostic  } *)

type version = { major:int; minor:int }

val version: major:int -> minor:int -> version
val pp: Format.formatter -> version -> unit

(** Lifetime description for fields and constructor *)
module Lifetime: sig

  (** Life cycle of fields and constructor in order *)
  type point =
    | Preview
     (** constructor only: whenever possible new constructors are introduced
         as preview before being published.*)
    | Publication
    | Expansion
      (** constructor only: argument expansion to a record *)
    | Deprecation
      (** record only? *)
    | Deletion
    | Future

  type t = {
    preview: version option;
    publication: version option;
    expansion: version option;
    deprecation: version option;
    deletion: version option
  }

  val make:
    ?deprecation:version ->
    ?deletion:version ->
    ?expansion:version ->
    ?published:bool ->
    version -> t

  (** [stage lifetime] is the current stage of the lifetime, or in other words
      the last field which is not [None]. *)
  val stage: t -> point

  (** [stage_at (Some v) lf] represents the stage of the lifetime at version
      [v]. [stage_at None lf] is [stage lf]. *)
  val stage_at: version option -> t -> point

  (** [at_version v lf] represents the lifetime seen at version [v] by
      removing all information ulterior to this version.*)
  val at_version: version -> t -> t option

end


(** {2:diagnostic_history History of versions }*)

(**  diagnostic history recording diagnostic changes across versions *)
type 'id t

(** Last version present in the history *)
val current_version: 'a t -> version

(** An ['id update] is a version registered in the history ['id t]. It is used
    as key for creating new record types, sum types or updating the contents of
    those types.*)
type 'a update
val new_version: 'a t -> version -> 'a update
(** [new_version h v] unconditionnally creates an update but registers an error
    in the history if the version is not a valid new version. *)

val v: 'a update -> version

(** {2 Versioning events }*)

(** Versionning policy violation *)
type error =
  | Duplicate_key of string
  | Time_travel of version * version
  | Inconsistent_change of Lifetime.t * string
  | Invalid_constructor_expansion of string
  | Invalid_publication of string
  | Sealed_version of version

(** Event in a diagnostic history *)
type base_event =
  | Declaration
  | Preview of {base_name:string; new_name:string; typ:string}
  | Publication of string
  | Creation of {name:string; typ:string}
  | Make_required of string
  | Expansion of {name:string; expansion:string}
  | Deprecation of string
  | Deletion of string
  | Seal
  | Error of error

(** Event in a history (for various diagnostics and versions ) *)
type event = { scheme: string; version:version; event:base_event }

(** Sequence of all events *)
val events: 'a t -> event Seq.t

(** [register_event u diag_name e] registers an event at update [u] for
    diagnostic [diag_name].*)
val register_event: 'a update -> string -> base_event -> unit
val error: 'a update -> string -> error -> unit
(** [error u diag_name e] is short-hand for registering an error event at
    [u]. *)

(** {2 Error }*)

(** [breaking_change u diag_name] registers an error if [u]* is not a major
    update.*)
val breaking_change: 'a update -> string -> unit

val inconsistent_if_not_deprecated:
  'a update -> scheme:string -> string -> Lifetime.t -> unit

val inconsistent_if_inactive:
  'a update -> scheme:string -> string -> Lifetime.t -> unit

val invalid_constructor_expansion:
  'a update -> scheme:string -> string -> unit

val invalid_publication: 'a update -> scheme:string -> string -> unit

module type S = sig
  type id
  val history: id t
  val new_version: version -> id update
end

(** Create a fresh history *)
module Make: functor () -> S
