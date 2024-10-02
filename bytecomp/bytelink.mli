(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

(* Link .cmo files and produce a bytecode executable. *)

module Dep : Set.OrderedType with
  type t = Cmo_format.compunit * Cmo_format.compunit
module DepSet : Set.S with type elt = Dep.t

val link : filepath list -> filepath -> unit
val reset : unit -> unit

val check_consistency: filepath -> Cmo_format.compilation_unit -> unit
val linkdeps_unit :
  Linkdeps.t -> filename:string -> Cmo_format.compilation_unit -> unit

val extract_crc_interfaces: unit -> crcs

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of modname * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
  | Camlheader of string * filepath
  | Link_error of Linkdeps.error
  | Needs_custom_runtime of filepath

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
