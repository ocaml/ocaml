(* Basic interface to the terminfo database *)

val setupterm: unit -> unit = "terminfo_setup"
val getstr: string -> string = "terminfo_getstr"
val getnum: string -> int = "terminfo_getnum"
val puts: out_channel -> string -> int -> unit = "terminfo_puts"

