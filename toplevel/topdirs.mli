(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* The toplevel directives. *)

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_cd : string -> unit
val dir_load : string -> unit
val dir_use : string -> unit
val dir_install_printer : Longident.t -> unit
val dir_remove_printer : Longident.t -> unit
val dir_trace : Longident.t -> unit
val dir_untrace : Longident.t -> unit
val dir_untrace_all : unit -> unit

val parse_toplevel_phrase : (Lexing.lexbuf -> Parsetree.toplevel_phrase) ref
val parse_use_file : (Lexing.lexbuf -> Parsetree.toplevel_phrase list) ref
val print_location : Location.t -> unit
