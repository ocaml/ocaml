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

(* Representation of automata *)

type automata =
    Perform of int
  | Shift of automata_trans * automata_move array
and automata_trans =
    No_remember
  | Remember of int
and automata_move =
    Backtrack
  | Goto of int

(* Representation of entry points *)

type automata_entry =
  { auto_name: string;
    auto_initial_state: int;
    auto_actions: (int * Syntax.location) list }

(* The entry point *)

val make_dfa: Syntax.lexer_definition -> automata_entry list * automata array
