(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Source code locations, used in parsetree *)

type t =
  { loc_start: int; loc_end: int }

val none: t
val symbol_loc: unit -> t
val rhs_loc: int -> t

val input_name: string ref
val input_lexbuf: Lexing.lexbuf option ref

val print: t -> unit
val print_warning: t -> string -> unit

