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

(* Id *)

(* Source code locations, used in parsetree *)
open Format

type t =
  { loc_start: int; loc_end: int; loc_ghost: bool }

val none: t
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

val highlight_locations: t -> t -> bool
