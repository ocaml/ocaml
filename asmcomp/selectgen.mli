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

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

type environment = (Ident.t, Reg.t array) Tbl.t

val size_expr : environment -> Cmm.expression -> int

class virtual selector_generic : object
  (* The following methods must or can be overriden by the processor
     description *)
  method virtual is_immediate : int -> bool
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to arithmetic instructions *)
  method virtual select_addressing :
    Cmm.expression -> Arch.addressing_mode * Cmm.expression
    (* Must be defined to select addressing modes *)
  method select_operation :
    Cmm.operation ->
    Cmm.expression list -> Mach.operation * Cmm.expression list
    (* Can be overriden to deal with special arithmetic instructions *)
  method select_condition : Cmm.expression -> Mach.test * Cmm.expression
    (* Can be overriden to deal with special test instructions *)
  method select_store :
    Arch.addressing_mode -> Cmm.expression -> Mach.operation * Cmm.expression
    (* Can be overriden to deal with special store constant instructions *)
  method insert_op :
    Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overriden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  method emit_extcall_args :
    environment -> Cmm.expression list -> Reg.t array * int
    (* Can be overriden to deal with stack-based calling conventions *)
  method emit_stores :
    environment -> Cmm.expression list -> Reg.t array -> unit
    (* Fill a freshly allocated block.  Can be overriden for architectures
       that do not provide Arch.offset_addressing. *)

  (* The following method is the entry point and should not be overriden *)
  method emit_fundecl : Cmm.fundecl -> Mach.fundecl
  
  (* The following methods should not be overriden.  They cannot be
     declared "private" in the current implementation because they
     are not always applied to "self", but ideally they should be private. *)
  method extract : Mach.instruction
  method insert : Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
  method insert_move : Reg.t -> Reg.t -> unit
  method insert_move_args : Reg.t array -> Reg.t array -> int -> unit
  method insert_move_results : Reg.t array -> Reg.t array -> int -> unit
  method insert_moves : Reg.t array -> Reg.t array -> unit
  method emit_expr :
    (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> Reg.t array option
  method emit_tail : (Ident.t, Reg.t array) Tbl.t -> Cmm.expression -> unit
end
