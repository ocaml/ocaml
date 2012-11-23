type t

val create : emit_string:(string -> unit)
  -> emit_symbol:(string -> unit)
  -> emit_label_declaration:(label_name:string -> unit)
  -> emit_section_declaration:(section_name:string -> unit)
  -> emit_switch_to_section:(section_name:string -> unit)
  -> t

val emit_string : t -> string -> unit
val emit_symbol : t -> string -> unit
val emit_label_declaration : t -> label_name:string -> unit
val emit_section_declaration : t -> section_name:string -> unit
val emit_switch_to_section : t -> section_name:string -> unit
