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

(* Compaction of an automata *)

type lex_tables =
  { tbl_base: int array;                 (* Perform / Shift *)
    tbl_backtrk: int array;              (* No_remember / Remember *)
    tbl_default: int array;              (* Default transition *)
    tbl_trans: int array;                (* Transitions (compacted) *)
    tbl_check: int array }               (* Check (compacted) *)

val compact_tables: Lexgen.automata array -> lex_tables
