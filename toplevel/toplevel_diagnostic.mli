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

include Compiler_diagnostic.Record

val output: Format_doc.doc optional_field
val backtrace: Format_doc.doc optional_field
val compiler: Compiler_diagnostic.id Diagnostic.record optional_field
val errors: Format_doc.doc list optional_field
val trace: Format_doc.doc list optional_field

val separate_new_message: id Log.t -> unit
