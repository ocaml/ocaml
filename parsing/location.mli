(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Source code locations (ranges of positions), used in parsetree. *)

open Format

type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

val none : t
(** An arbitrary value of type [t]; describes an empty ghost range. *)
val init : Lexing.lexbuf -> string -> unit
(** Set the file name and line number of the [lexbuf] to be the start
    of the named file. *)
val curr : Lexing.lexbuf -> t
(** Get the location of the current token from the [lexbuf]. *)

val symbol_rloc: unit -> t
val symbol_gloc: unit -> t
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val print: formatter -> t -> unit
val print_warning: t -> formatter -> Warnings.t -> unit
val prerr_warning: t -> Warnings.t -> unit
val echo_eof: unit -> unit
val reset: unit -> unit

val highlight_locations: formatter -> t -> t -> bool
