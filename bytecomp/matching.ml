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



let compile_matching =
  if Matchcommon.tree then Treematch.compile_matching
  else begin
    (fun loc repr hf param pat_act_list partial ->
      nswitch := 0 ;
      ignore
	(Treematch.compile_matching loc repr hf param pat_act_list partial);
      let ntree = !nswitch in
      nswitch := 0 ;
      let r =
	Automatch.compile_matching  loc repr hf param pat_act_list partial in
      let nauto = !nswitch in
      if ntree <> nauto then begin
	Location.print_error Format.err_formatter loc ;
	Format.eprintf "Signal: %i > %i@." ntree nauto ;
      end ;
      r)
  end

let for_function loc repr param pat_act_list partial =
  match pat_act_list with
  | [{pat_desc=Tpat_var id},e] ->
      bind Alias id param e
  |  [{pat_desc=Tpat_any},e] ->
      e
  | _ ->
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
  if Matchcommon.tree then Treematch.for_multiple_match
  else
    (fun loc args pss partial ->
      nswitch := 0 ;
      ignore
	(Treematch.for_multiple_match loc args pss partial);
      let ntree = !nswitch in
      nswitch := 0 ;
      let r =
	Automatch.for_multiple_match loc args pss partial in
      let nauto = !nswitch in
      if ntree <> nauto then begin
	Location.print_error Format.err_formatter loc ;
	Format.eprintf "Signal: %i > %i@." ntree nauto ;
      end ;
      r)

let for_tupled_function =
  if Matchcommon.tree then
    Treematch.for_tupled_function
  else
    Automatch.for_tupled_function

(* Re-exported *)
exception Cannot_flatten = Matchcommon.Cannot_flatten

let flatten_pattern = Matchcommon.flatten_pattern

let make_test_sequence = Matchcommon.make_test_sequence
