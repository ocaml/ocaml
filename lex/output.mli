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

(* Output the DFA tables and its entry points *)

val output_lexdef:
      string -> in_channel -> out_channel ->
      Syntax.location ->
      Compact.lex_tables ->
      Lexgen.automata_entry list ->
      Syntax.location ->
      unit

exception Table_overflow
