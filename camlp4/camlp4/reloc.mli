(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

value zero_loc : Lexing.position;
value shift_pos : int -> Lexing.position -> Lexing.position;
value adjust_loc : Lexing.position -> MLast.loc -> MLast.loc;
value linearize : MLast.loc -> MLast.loc;
value patt : (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.patt -> MLast.patt;
value expr : (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.expr -> MLast.expr;
