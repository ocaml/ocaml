(* Processor descriptions *)

(* The Use_default exception is raised by the selection and reloading
   functions to signal cases they don't handle *)
exception Use_default

(* Instruction selection *)
val select_addressing:
      Cmm.expression -> Arch.addressing_mode * Cmm.expression
val select_oper:
      Cmm.operation -> Cmm.expression list ->
      Mach.operation * Cmm.expression
val pseudoregs_for_operation:
      Mach.operation -> Reg.t array -> Reg.t array ->
        Reg.t array * Reg.t array
val is_immediate: int -> bool

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

(* Registers destroyed by operations *)
val destroyed_at_oper: Mach.instruction_desc -> Reg.t array
val destroyed_at_call: Reg.t array
val destroyed_at_extcall: Reg.t array
val destroyed_at_raise: Reg.t array

(* Reloading of instruction arguments, storing of instruction results *)
val reload_test: (Reg.t -> Reg.t) -> Mach.test -> Reg.t array -> Reg.t array
val reload_operation:
      (Reg.t -> Reg.t) -> Mach.operation -> Reg.t array -> Reg.t array ->
        Reg.t array * Reg.t array

(* Layout of the stack frame *)
val num_stack_slots: int array
val stack_offset: int ref
val contains_calls: bool ref
val frame_size: unit -> int
val slot_offset: Reg.stack_location -> int -> int
