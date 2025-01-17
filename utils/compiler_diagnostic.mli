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

module D := Diagnostic

(** Compiler diagnostic version line *)
module V: Diagnostic_history.S
val v1: V.id Diagnostic_history.update
module type Record = D.Record with type vl := V.id
module type Sum = D.Sum with type vl := V.id

type doc = Format_doc.doc

module Structured_text: sig
  module Format_tag: Sum
  type _ D.extension += Doc: Format_doc.Doc.t D.extension

  (** [register_tag0 stag] add a new constructor of arity [0] to [Format_tag]
      for a [Format.stag] constructor. *)
  val register_tag0:
    V.id Diagnostic.update -> Obj.Extension_constructor.t
    -> unit

    (** [register_tag tag conv] registers a translation function for a
      [Format.stag] argument to a pre-existing [Format_tag] variant. *)
  val register_tag:
    Obj.Extension_constructor.t
    -> (Diagnostic.version option -> Format.stag -> Format_tag.id D.sum)
    -> unit

  val typ: doc D.typ
end

(** Debugging output enabled with [-d...] flags (e.g [-dsource]) *)
module Debug: sig
  include Record
  val source: string field
  val parsetree: string field
  val typedtree: string field
  val shape: string field
  val instr: string field
  val raw_lambda: string field
  val lambda: string field
  val flambda: string list field
  val raw_flambda: string list field
  val clambda: string list field
  val raw_clambda: string list field
  val cmm: string list field
  val remove_free_vars_equal_to_args: string list field
  val unbox_free_vars_of_closures: string list field
  val unbox_closures:string list field
  val unbox_specialised_args:string list  field
  val mach: string list field
  val linear: string list field
  val cmm_invariant: string field
end

(** Error report record, the related fields are defined in {!Location} *)
module Error: Record

include Record
val debug: Debug.id D.record field
val doc: Format_doc.t Diagnostic.typ
val ldoc: Format_doc.t list Diagnostic.typ
