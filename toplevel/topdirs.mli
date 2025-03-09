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

(* The toplevel directives. *)

open Format

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_remove_directory : string -> unit
val dir_cd : string -> unit
val dir_load : formatter -> string -> unit
val dir_use : formatter -> string -> unit
val dir_use_output : formatter -> string -> unit
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit

(* These are now injected from [Topeval], for the bytecode toplevel only:
val dir_trace : formatter -> Longident.t -> unit
val dir_untrace : formatter -> Longident.t -> unit
val dir_untrace_all : formatter -> unit -> unit
 *)

val section_general : string
val section_run : string
val section_env : string

val section_print : string
val section_trace : string
val section_options : string

val section_undocumented : string

(* Here for backwards compatibility, use [Toploop.load_file]. *)
val[@deprecated] load_file : formatter -> string -> bool
