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

(* The lexical analyzer *)

val token: Lexing.lexbuf -> Parser.token

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error * int * int

val report_error: error -> unit

