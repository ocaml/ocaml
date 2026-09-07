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

(** Compiler diagnostic version line *)
module V: Diagnostic_history.S
val v1: V.id Diagnostic_history.update
module type Record = Diagnostic.Record with type vl := V.id
module type Sum = Diagnostic.Sum with type vl := V.id

type doc = Format_doc.doc

module Structured_text: sig
  module Format_tag: Sum
  type _ Diagnostic.extension += Doc: Format_doc.Doc.t Diagnostic.extension

  (** [register_tag0 stag] add a new constructor of arity [0] to [Format_tag]
      for a [Format.stag] constructor. *)
  val register_tag0:
    V.id Diagnostic.update -> Obj.Extension_constructor.t
    -> unit

    (** [register_tag tag conv] registers a translation function for a
      [Format.stag] argument to a pre-existing [Format_tag] variant. *)
  val register_tag:
    Obj.Extension_constructor.t
    -> ( Diagnostic.version option -> Format.stag
        -> Format_tag.id Diagnostic.sum )
    -> unit

  val typ: doc Diagnostic.typ
end

(** Debugging output enabled with [-d...] flags (e.g [-dsource]).
    The related fields are defined in Dev_log*)
module Dev: Record

(** Error report record, the related fields are defined in {!Location} *)
module Error: Record

include Record
val dev: Dev.id Diagnostic.record optional_field
val doc: Format_doc.t Diagnostic.typ
val ldoc: Format_doc.t list Diagnostic.typ
