(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Schedgen (* to create a dependency *)

(* No scheduling is needed for the Alpha, the Digital Unix assembler
   does it better than us.  Problem: the assembler for Linux-Alpha
   does not do scheduling... *)

let fundecl f = f
