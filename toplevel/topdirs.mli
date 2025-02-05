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

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_remove_directory : string -> unit
val dir_cd : string -> unit

module V2: sig
  val dir_load : Toplevel_diagnostic.id Log.t -> string -> unit
  val dir_use : Toplevel_diagnostic.id Log.t -> string -> unit
  val dir_use_output : Toplevel_diagnostic.id Log.t -> string -> unit
  val dir_install_printer : Toplevel_diagnostic.id Log.t -> Longident.t -> unit
  val dir_remove_printer : Toplevel_diagnostic.id Log.t -> Longident.t -> unit
end
val dir_load : Format.formatter -> string -> unit
val dir_use : Format.formatter -> string -> unit
val dir_use_output : Format.formatter -> string -> unit
val dir_install_printer : Format.formatter -> Longident.t -> unit
val dir_remove_printer : Format.formatter -> Longident.t -> unit



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
val[@deprecated] load_file :  Toplevel_diagnostic.id Log.t -> string -> bool
