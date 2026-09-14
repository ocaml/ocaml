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
  val source: string optional_field
  val parsetree: string optional_field
  val typedtree: string optional_field
  val shape: string optional_field
  val instr: string optional_field
  val raw_lambda: string optional_field
  val lambda: string optional_field
  val flambda: string list optional_field
  val raw_flambda: string list optional_field
  val clambda: string list optional_field
  val raw_clambda: string list optional_field
  val cmm: string list optional_field
  val remove_free_vars_equal_to_args: string list optional_field
  val unbox_free_vars_of_closures: string list optional_field
  val unbox_closures:string list optional_field
  val unbox_specialised_args:string list  optional_field
  val mach: string list optional_field
  val linear: string list optional_field
  val cmm_invariant: string optional_field
end

(** Error report record, the related fields are defined in {!Location} *)
module Error: Record

include Record
val debug: Debug.id D.record optional_field
val doc: Format_doc.t Diagnostic.typ
val ldoc: Format_doc.t list Diagnostic.typ
