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

(** Log validation for non-streaming {!Log.t}*)

type version =
  | Downward_compatible of Diagnostic_history.version
    (** Log emitted at version [Downward_compatible v] should make the emitted
        log as downward compatible as possible. *)
  | Exact of Diagnostic_history.version
    (** Log emitted at version [Exact v] only keep data valid and non-deprecated
        at version [v]. *)

val reference_version: version -> Diagnostic_history.version
val exact_version: version -> Diagnostic_history.version option

(** A path of field inside a diagnostic *)
type path = string list

(** A report of deprecated or invalid fields or constructors. *)
type report_paths = { deprecated: path list; invalid: path list }

(** Diagnostic validation: [diagnostic ~version diag r] check that the record
    [r] is a valid diagnostic viewed at version [v], add the metadata field with
    the relevant information (version, validity, error reports) and possibly
    reports invalid or deprecated fields. *)
val diagnostic:
  version:version -> 'a Diagnostic.t -> 'a Diagnostic.record -> report_paths
