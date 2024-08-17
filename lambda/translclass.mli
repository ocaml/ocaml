(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Typedtree
open Lambda
open Debuginfo.Scoped_location

val transl_class :
  scopes:scopes -> Ident.t list -> Ident.t ->
  string list -> class_expr -> Asttypes.virtual_flag ->
  lambda * Value_rec_types.recursive_binding_kind

type error = Tags of string * string

exception Error of Location.t * error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
