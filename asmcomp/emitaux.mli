(* Common functions for emitting assembly code *)

val output_channel: out_channel ref
val emit_string: string -> unit
val emit_int: int -> unit
val emit_symbol: string -> unit
val emit_string_literal: string -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
