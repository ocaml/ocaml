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
