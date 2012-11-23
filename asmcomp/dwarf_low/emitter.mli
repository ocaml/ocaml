type t

val create : emit_string:(string -> unit)
  -> emit_symbol:(string -> unit)
  -> emit_label_declaration:(label_name:string -> unit)
  -> t

val emit_string : t -> string -> unit
val emit_symbol : t -> string -> unit
val emit_label_declaration : t -> label_name:string -> unit
