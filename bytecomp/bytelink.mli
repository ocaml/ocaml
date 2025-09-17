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

(** Ways of starting a bytecode executable *)
type launch_method =
| Shebang_bin_sh of string (** Use a shell script *)
| Shebang_runtime          (** Invoke the runtime directly *)
| Executable               (** Use the executable stub *)

(** runtime-launch-info files *)
type runtime_launch_info = {
  buffer : string;          (** Content of the file *)
  bindir : string;          (** Directory containing runtime executables *)
  launcher : launch_method; (** Default launch method (this is never
                                {!Shebang_runtime}) *)
  executable_offset : int   (** Offset in the buffer field at which the
                                executable stub data begins *)
}

val read_runtime_launch_info : string -> runtime_launch_info
(** [read_runtime_launch_info file] loads the {!runtime_launch_info} from [file]

    @raise Error if the file cannot be parsed *)

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
