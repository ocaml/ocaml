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

(* Processor descriptions *)

(* The Use_default exception is raised by the selection and reloading
   functions to signal cases they don't handle *)
exception Use_default

(* Instruction selection *)
val select_addressing:
      Cmm.expression -> Arch.addressing_mode * Cmm.expression
val select_oper:
      Cmm.operation -> Cmm.expression list ->
        Mach.operation * Cmm.expression list
val select_store:
      Arch.addressing_mode -> Cmm.expression -> Mach.operation * Cmm.expression
val pseudoregs_for_operation:
      Mach.operation -> Reg.t array -> Reg.t array ->
        Reg.t array * Reg.t array
val is_immediate: int -> bool
val word_addressed: bool

(* Registers available for register allocation *)
val num_register_classes: int
val register_class: Reg.t -> int
val num_available_registers: int array
val first_available_register: int array
val register_name: int -> string
val phys_reg: int -> Reg.t

(* Calling conventions *)
val loc_arguments: Reg.t array -> Reg.t array * int
val loc_results: Reg.t array -> Reg.t array
val loc_parameters: Reg.t array -> Reg.t array
val loc_external_arguments: Reg.t array -> Reg.t array * int
val loc_external_results: Reg.t array -> Reg.t array
val loc_exn_bucket: Reg.t

(* Maximal register pressures for pre-spilling *)

val safe_register_pressure: Mach.operation -> int
val max_register_pressure: Mach.operation -> int array

(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
val destroyed_at_raise: Reg.t array

(* Reloading of instruction arguments, storing of instruction results *)
val reload_test:
      (Reg.t -> Reg.t) -> Mach.test -> Reg.t array -> Reg.t array
val reload_operation:
      (Reg.t -> Reg.t) -> Mach.operation -> Reg.t array -> Reg.t array ->
        Reg.t array * Reg.t array

(* Info for laying out the stack frame *)
val num_stack_slots: int array
val contains_calls: bool ref

(* Calling the assembler and the archiver *)
val assemble_file: string -> string -> int
val create_archive: string -> string list -> int
