(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Conversion of streams and parsers into Caml-Special-Light syntax *)

open Parsetree

type stream_pattern_component =
    Spat_term of pattern
  | Spat_nterm of pattern * expression
  | Spat_sterm of pattern
type stream_expr_component =
    Sexp_term of expression
  | Sexp_nterm of expression

val cparser :
  pattern option *
    ((stream_pattern_component * expression option) list *
     pattern option *
     expression) list ->
  expression
val cstream : stream_expr_component list -> expression
