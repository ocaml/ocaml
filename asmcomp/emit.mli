(* Generation of assembly code *)

val fundecl: Linearize.fundecl -> unit
val data: Cmm.data_item list -> unit
val begin_assembly: unit -> unit
val end_assembly: unit -> unit
val fastcode_flag: bool ref
