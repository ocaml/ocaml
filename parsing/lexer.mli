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

(* The lexical analyzer *)

val token: Lexing.lexbuf -> Parser.token

type error =
  | Illegal_character
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
;;

exception Error of error * int * int

val report_error: error -> unit
