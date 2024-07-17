(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmo files into one .cmo file having the
   original compilation units as sub-modules. *)

val package_files:
  ppf_dump:Format.formatter -> Env.t -> string list -> string -> unit

type error =
    Forward_reference of string * Cmo_format.compunit
  | Multiple_definition of string * Cmo_format.compunit
  | Not_an_object_file of string
  | Illegal_renaming of Cmo_format.compunit * string * Cmo_format.compunit
  | File_not_found of string

exception Error of error

val report_error: error Format_doc.format_printer
val report_error_doc: error Format_doc.printer
