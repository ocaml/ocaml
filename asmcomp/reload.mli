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

(* Insert load/stores for pseudoregs that got assigned to stack locations.
   Insert moves to comply with calling conventions, etc. *)

val fundecl: Mach.fundecl -> Mach.fundecl * bool

(* Auxiliary functions for use by the processor description to do its own
   reloading *)

val makereg: Reg.t -> Reg.t
val makeregs: Reg.t array -> Reg.t array
