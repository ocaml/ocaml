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

#directory "../utils";;
#directory "../typing";;
#load "../utils/misc.cmo";;
#load "../utils/tbl.cmo";;
#load "../typing/ident.cmo";;
#load "arch.cmo";;
#load "cmm.cmo";;
#load "printcmm.cmo";;
#load "reg.cmo";;
#load "mach.cmo";;
#load "proc.cmo";;
(*********
#load "printmach.cmo";;
#load "selection.cmo";;
#load "sequence.cmo";;
#load "liveness.cmo";;
#load "spill.cmo";;
#load "split.cmo";;
#load "interf.cmo";;
#load "coloring.cmo";;
#load "reload.cmo";;
#load "linearize.cmo";;
#load "emitaux.cmo";;
#load "emit.cmo";;
#load "parsecmmaux.cmo";;
#load "parsecmm.cmo";;
#load "lexcmm.cmo";;
#load "codegen.cmo";;
***********)
