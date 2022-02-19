(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

type environment

val env_add
   : ?mut:Asttypes.mutable_flag
  -> Backend_var.With_provenance.t
  -> Reg.t array
  -> environment
  -> environment

val env_find : Backend_var.t -> environment -> Reg.t array

val size_expr : environment -> Cmm.expression -> int

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary
end

module Effect_and_coeffect : sig
  type t

  val none : t
  val arbitrary : t

  val effect : t -> Effect.t
  val coeffect : t -> Coeffect.t

  val effect_only : Effect.t -> t
  val coeffect_only : Coeffect.t -> t

  val join : t -> t -> t
  val join_list_map : 'a list -> ('a -> t) -> t
end

type selector_state

type selector = {
  (* The following methods must or can be overridden by the processor
     description *)
  is_immediate : selector -> Mach.integer_operation -> int -> bool;
    (* Must be overridden to indicate whether a constant is a suitable
       immediate operand to the given integer arithmetic instruction.
       The default implementation handles shifts by immediate amounts,
       but produces no immediate operations otherwise. *)
  is_immediate_test : selector -> Mach.integer_comparison -> int -> bool;
    (* Must be defined to indicate whether a constant is a suitable
       immediate operand to the given integer test *)
  select_addressing : selector ->
    Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression;
    (* Must be defined to select addressing modes *)
  is_simple_expr: selector -> Cmm.expression -> bool;
  effects_of : selector -> Cmm.expression -> Effect_and_coeffect.t;
    (* Can be overridden to reflect special extcalls known to be pure *)
  select_operation : selector ->
    Cmm.operation ->
    Cmm.expression list ->
    Debuginfo.t ->
    Mach.operation * Cmm.expression list;
    (* Can be overridden to deal with special arithmetic instructions *)
  select_condition : selector -> Cmm.expression -> Mach.test * Cmm.expression;
    (* Can be overridden to deal with special test instructions *)
  select_store : selector ->
    bool -> Arch.addressing_mode -> Cmm.expression ->
                                         Mach.operation * Cmm.expression;
    (* Can be overridden to deal with special store constant instructions *)
  regs_for : selector -> Cmm.machtype -> Reg.t array;
    (* Return an array of fresh registers of the given type.
       Default implementation is like Reg.createv.
       Can be overridden if float values are stored as pairs of
       integer registers. *)
  insert_op : selector ->
    environment -> Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array;
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  insert_op_debug : selector ->
    environment -> Mach.operation -> Debuginfo.t -> Reg.t array
      -> Reg.t array -> Reg.t array;
    (* Can be overridden to deal with 2-address instructions
       or instructions with hardwired input/output registers *)
  insert_move_extcall_arg : selector ->
    environment -> Cmm.exttype -> Reg.t array -> Reg.t array -> unit;
    (* Can be overridden to deal with unusual unboxed calling conventions,
       e.g. on a 64-bit platform, passing unboxed 32-bit arguments
       in 32-bit stack slots. *)
  emit_extcall_args : selector ->
    environment -> Cmm.exttype list -> Cmm.expression list -> Reg.t array * int;
    (* Can be overridden to deal with stack-based calling conventions *)
  emit_stores : selector ->
    environment -> Cmm.expression list -> Reg.t array -> unit;
    (* Fill a freshly allocated block.  Can be overridden for architectures
       that do not provide Arch.offset_addressing. *)

  mark_call : selector -> unit;
  (* informs the code emitter that the current function is non-leaf:
     it may perform a (non-tail) call; by default, sets
     [contains_calls := true] *)

  mark_tailcall : selector -> unit;
  (* informs the code emitter that the current function may end with
     a tail-call; by default, does nothing *)

  mark_c_tailcall : selector -> unit;
  (* informs the code emitter that the current function may call
     a C function that never returns; by default, does nothing.

     It is unnecessary to save the stack pointer in this situation
     (which is the main purpose of tracking leaf functions) but some
     architectures still need to ensure that the stack is properly
     aligned when the C function is called. This is achieved by
     overloading this method to set [contains_calls := true] *)

  mark_instr : selector -> Mach.instruction_desc -> unit;
  (* dispatches on instructions to call one of the marking function
     above; overloading this is useful if Ispecific instructions need
     marking *)

  (* [contains_calls] is declared as a reference instance variable,
     instead of a mutable boolean instance variable,
     because the traversal uses functional object copies. *)
  contains_calls : bool ref;
  mutable instr_seq : selector_state;
}

val default_selector : selector

val extract_onto : selector -> Mach.instruction -> Mach.instruction
val extract : selector -> Mach.instruction
val insert : selector ->
    environment -> Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit
val insert_debug : selector ->
    environment -> Mach.instruction_desc -> Debuginfo.t ->
      Reg.t array -> Reg.t array -> unit
val insert_move : selector -> environment -> Reg.t -> Reg.t -> unit
val insert_move_args : selector ->
    environment -> Reg.t array -> Reg.t array -> int -> unit
val insert_move_results : selector ->
    environment -> Reg.t array -> Reg.t array -> int -> unit
val insert_moves : selector -> environment -> Reg.t array -> Reg.t array -> unit
val emit_expr : selector ->
    environment -> Cmm.expression -> Reg.t array option
val emit_tail : selector -> environment -> Cmm.expression -> unit

(* Entry point *)
val emit_fundecl : selector ->
      future_funcnames:Misc.Stdlib.String.Set.t -> Cmm.fundecl -> Mach.fundecl

val reset : unit -> unit
