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

(* Pretty-printing of pseudo machine code *)

val reg: Reg.t -> unit
val regs: Reg.t array -> unit
val regset: Reg.Set.t -> unit
val regsetaddr: Reg.Set.t -> unit
val operation: Mach.operation -> Reg.t array -> Reg.t array -> unit
val test: Mach.test -> Reg.t array -> unit
val instr: Mach.instruction -> unit
val fundecl: Mach.fundecl -> unit
val phase: string -> Mach.fundecl -> unit
val interferences: unit -> unit
val preferences: unit -> unit

val print_live: bool ref
