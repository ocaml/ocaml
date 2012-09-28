(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: syntax.ml 12858 2012-08-10 14:45:51Z maranget $ *)

(* This apparently useless implmentation file is in fact required
   by the pa_ocamllex syntax extension *)

(* The shallow abstract syntax *)

type location =
    { start_pos: int;
      end_pos: int;
      start_line: int;
      start_col: int }

type regular_expression =
    Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression * (string * location)

type ('arg,'action) entry =
  {name:string ;
   shortest : bool ;
   args : 'arg ;
   clauses : (regular_expression * 'action) list}

type  lexer_definition =
    { header: location;
      entrypoints: ((string list, location) entry) list;
      trailer: location }
