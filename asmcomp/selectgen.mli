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

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

type environment = (Ident.t, Reg.t array) Tbl.t

val size_expr : environment -> Cmm.expression -> int

class virtual selector_generic (unit) : 'a =
  (* The following methods must or can be overriden by the processor
     description *)
  virtual is_immediate : int -> bool
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to arithmetic instructions *)
  virtual select_addressing :
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

  (* The following methods should not be overriden *)
  method emit_expr :
    environment -> Cmm.expression -> Reg.t array
  method emit_fundecl : Cmm.fundecl -> Mach.fundecl
  method emit_let :
    environment ->
    Ident.t -> Cmm.expression -> environment
  method emit_parts :
    environment ->
    Cmm.expression -> Cmm.expression * environment
  method emit_parts_list :
    environment ->
    Cmm.expression list -> Cmm.expression list * environment
  method emit_return : environment -> Cmm.expression -> unit
  method emit_sequence :
    environment -> Cmm.expression -> Reg.t array * 'a
  method emit_stores :
    environment ->
    Cmm.expression list -> Reg.t array -> Arch.addressing_mode -> unit
  method emit_tail : environment -> Cmm.expression -> unit
  method emit_tail_sequence :
    environment -> Cmm.expression -> Mach.instruction
  method emit_tuple :
    environment -> Cmm.expression list -> Reg.t array
  method extract : Mach.instruction
  method insert : Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
  method insert_move : Reg.t -> Reg.t -> unit
  method insert_move_args : Reg.t array -> Reg.t array -> int -> unit
  method insert_move_results : Reg.t array -> Reg.t array -> int -> unit
  method insert_moves : Reg.t array -> Reg.t array -> unit
  method select_arith :
    Mach.integer_operation ->
    Cmm.expression list -> Mach.operation * Cmm.expression list
  method select_arith_comm :
    Mach.integer_operation ->
    Cmm.expression list -> Mach.operation * Cmm.expression list
  method select_arith_comp :
    Mach.integer_comparison ->
    Cmm.expression list -> Mach.operation * Cmm.expression list
  method select_shift :
    Mach.integer_operation ->
    Cmm.expression list -> Mach.operation * Cmm.expression list
end

