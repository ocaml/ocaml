(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*    Valerie Menissier-Morain, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let error_when_null_denominator_flag = ref true;;

let normalize_ratio_flag = ref false;;

let normalize_ratio_when_printing_flag = ref true;;

let floating_precision = ref 12;;

let approx_printing_flag = ref false;;

