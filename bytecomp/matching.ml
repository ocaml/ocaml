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
open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Lambda
open Parmatch
open Printf
open Matchcommon

let treematch =
  try ignore (Sys.getenv "TREEMATCH") ; true with Not_found -> false


let compile_matching =
  if treematch then Treematch.compile_matching
  else Automatch.compile_matching

let for_function loc repr param pat_act_list partial =
  compile_matching
    loc repr (partial_function loc) param pat_act_list partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith param pat_act_list =
  compile_matching Location.none None (fun () -> Lprim(Praise, [param]))
    param pat_act_list Partial

let for_let loc param pat body = 
  compile_matching loc None (partial_function loc) param [pat, body] Partial

(* Those two are more complex *)
let for_multiple_match =
  if treematch then Treematch.for_multiple_match
  else Automatch.for_multiple_match

let for_tupled_function =
  if treematch then Treematch.for_tupled_function
  else Automatch.for_tupled_function

(* Re-exported *)
exception Cannot_flatten = Matchcommon.Cannot_flatten

let flatten_pattern = Matchcommon.flatten_pattern

let make_test_sequence = Matchcommon.make_test_sequence
