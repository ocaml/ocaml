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

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Misc
open Format

val link: ppf_dump:formatter -> string list -> string -> unit

val link_shared: ppf_dump:formatter -> string list -> string -> unit

val call_linker_shared: string list -> string -> unit

val reset : unit -> unit
val check_consistency: filepath -> Cmx_format.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> crcs
val extract_crc_implementations: unit -> crcs

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname

exception Error of error

val report_error: formatter -> error -> unit
