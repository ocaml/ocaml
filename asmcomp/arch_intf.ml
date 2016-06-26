module type S = sig
  val command_line_options: (string * Arg.spec * string) list

  type addressing_mode
  type specific_operation

  val big_endian: bool
  val size_addr: int
  val size_int: int
  val size_float: int
  val allow_unaligned_access: bool
  val division_crashes_on_overflow: bool
  val identity_addressing: addressing_mode
  val offset_addressing: addressing_mode -> int -> addressing_mode
  val num_args_addressing: addressing_mode -> int

  val print_addressing: (Format.formatter -> Reg.t -> unit) -> addressing_mode -> Format.formatter -> Reg.t array -> unit
  val print_specific_operation: (Format.formatter -> Reg.t -> unit) -> specific_operation -> Format.formatter -> Reg.t array -> unit
end
