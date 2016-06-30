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

module type S = sig
  type addressing_mode
  type specific_operation

  val word_addressed: bool
  val num_register_classes: int
  val register_class: Reg.t -> int
  val num_available_registers: int array
  val first_available_register: int array
  val register_name: int -> string
  val phys_reg: int -> Reg.t
  val rotate_registers: bool
  val loc_arguments: Reg.t array -> Reg.t array * int
  val loc_results: Reg.t array -> Reg.t array
  val loc_parameters: Reg.t array -> Reg.t array
  val loc_external_arguments: Reg.t array array -> Reg.t array array * int
  val loc_external_results: Reg.t array -> Reg.t array
  val loc_exn_bucket: Reg.t
  val max_arguments_for_tailcalls : int
  val safe_register_pressure: (addressing_mode, specific_operation) Mach.operation -> int
  val max_register_pressure: (addressing_mode, specific_operation) Mach.operation -> int array
  val destroyed_at_oper: (addressing_mode, specific_operation) Mach.instruction_desc -> Reg.t array
  val destroyed_at_raise: Reg.t array
  val regs_are_volatile: Reg.t array -> bool
  val op_is_pure: (addressing_mode, specific_operation) Mach.operation -> bool
  val num_stack_slots: int array
  val contains_calls: bool ref
  val assemble_file: string -> string -> int
  val init : unit -> unit
end
